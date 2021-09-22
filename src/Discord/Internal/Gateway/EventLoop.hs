{-# LANGUAGE OverloadedStrings #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Realistically, this is probably lower level than most
--   people will need
module Discord.Internal.Gateway.EventLoop where

import           Prelude                 hiding ( log )

import           Control.Concurrent             ( forkIO
                                                , killThread
                                                , threadDelay
                                                )
import           Control.Concurrent.Async       ( race )
import           Control.Concurrent.Chan
import           Control.Exception.Safe         ( SomeException
                                                , finally
                                                , handle
                                                , try
                                                )
import           Control.Monad                  ( forever )
import           Data.Aeson                     ( eitherDecode
                                                , encode
                                                )
import qualified Data.ByteString.Lazy          as BL
import           Data.IORef
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           System.Random                  ( getStdRandom
                                                , randomR
                                                )

import           Network.WebSockets             ( Connection
                                                , ConnectionException(..)
                                                , receiveData
                                                , sendTextData
                                                )
import           Wuss                           ( runSecureClient )

import           Discord.Internal.Types

data GatewayException = GatewayExceptionCouldNotConnect T.Text
                      | GatewayExceptionEventParseError T.Text T.Text
                      | GatewayExceptionUnexpected GatewayReceivable T.Text
                      | GatewayExceptionConnection ConnectionException T.Text
  deriving (Show)

data ConnLoopState = ConnStart
                   | ConnClosed
                   | ConnReconnect Auth T.Text Integer
  deriving Show

-- | Securely run a connection IO action. Send a close on exception
connect :: (Connection -> IO a) -> IO a
connect = runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json"

type DiscordHandleGateway
  = ( Chan (Either GatewayException Event)
    , Chan GatewaySendable
    , IORef (Maybe UpdateStatusOpts)
    )

connectionLoop :: Auth -> DiscordHandleGateway -> Chan T.Text -> IO ()
connectionLoop auth (events, userSend, lastStatus) log = loop ConnStart 0
 where
  loop :: ConnLoopState -> Int -> IO ()
  loop s retries = case s of
    ConnClosed -> pure ()
    ConnStart  -> do
        -- only try-catch an IO Error
      next <- try $ connect $ \conn -> do
        msg <- getPayload conn log
        case msg of
          Right (Hello interval) -> do
            sendTextData conn (encode (Identify auth False 50 (0, 1)))
            msg2 <- getPayload conn log
            case msg2 of
              Right (Dispatch r@(Ready _ _ _ _ seshID) _) -> do
                writeChan events (Right r)
                startEventStream (ConnData conn seshID auth events)
                                 interval
                                 0
                                 userSend
                                 lastStatus
                                 log
              Right m -> do
                writeChan
                  events
                  (Left
                    (GatewayExceptionUnexpected
                      m
                      "Response to Identify must be Ready"
                    )
                  )
                pure ConnClosed
              Left (CloseRequest code _str) -> do
                writeChan log ("gateway - close " <> T.pack (show code))
                threadDelay (3 * (10 ^ (6 :: Int)))
                pure ConnStart
              Left ce -> do
                writeChan
                  events
                  (Left (GatewayExceptionConnection ce "Response to Identify"))
                pure ConnClosed
          Right m -> do
            writeChan
              log
              ("gateway - first message must be hello: " <> T.pack (show msg))
            writeChan
              events
              (Left
                (GatewayExceptionUnexpected
                  m
                  "Response to connecting must be hello"
                )
              )
            pure ConnClosed
          Left (CloseRequest code _str) -> do
            writeChan log ("gateway - close " <> T.pack (show code))
            threadDelay (3 * (10 ^ (6 :: Int)))
            pure ConnStart
          Left ce -> do
            writeChan
              events
              (Left (GatewayExceptionConnection ce "Response to connecting"))
            pure ConnClosed
      case next :: Either SomeException ConnLoopState of
        Left _ -> do
          writeChan
            events
            (Left
              (GatewayExceptionCouldNotConnect
                "SomeException in gateway Connection"
              )
            )
          loop ConnClosed 0
        Right n -> loop n 0

    (ConnReconnect (Auth tok) seshID seqID) -> do
      next <- try $ connect $ \conn -> do
        sendTextData conn (encode (Resume tok seshID seqID))
        eitherPayload <- getPayload conn log
        case eitherPayload of
          Right (Hello interval) -> startEventStream
            (ConnData conn seshID auth events)
            interval
            seqID
            userSend
            lastStatus
            log
          Right (InvalidSession retry) -> do
            t <- getStdRandom $ randomR (1, 5)
            threadDelay (t * (10 ^ (6 :: Int)))
            pure $ if retry
              then ConnReconnect (Auth tok) seshID seqID
              else ConnStart
          Right payload -> do
            writeChan
              events
              (Left
                (GatewayExceptionUnexpected
                  payload
                  "Response to Resume must be Hello/Invalid Session"
                )
              )
            pure ConnClosed
          Left (CloseRequest code _str) -> do
            writeChan log ("gateway - retrying from " <> T.pack (show code))
            threadDelay (3 * (10 ^ (6 :: Int)))
            pure (ConnReconnect (Auth tok) seshID seqID)
          Left e -> do
            writeChan
              events
              (Left (GatewayExceptionConnection e "Could not ConnReconnect"))
            pure ConnClosed
      case next :: Either SomeException ConnLoopState of
        Left _ -> do
          t <- getStdRandom $ randomR (3, 20)
          threadDelay (t * (10 ^ (6 :: Int)))
          writeChan
            log
            (  "gateway - trying to reconnect after "
            <> T.pack (show retries)
            <> " failures"
            )
          loop (ConnReconnect (Auth tok) seshID seqID) (retries + 1)
        Right n -> loop n 1


getPayloadTimeout
  :: Connection
  -> Int
  -> Chan T.Text
  -> IO (Either ConnectionException GatewayReceivable)
getPayloadTimeout conn interval log = do
  res <- race (threadDelay ((interval * 1000 * 3) `div` 2))
              (getPayload conn log)
  case res of
    Left  ()    -> pure (Right Reconnect)
    Right other -> pure other

getPayload
  :: Connection
  -> Chan T.Text
  -> IO (Either ConnectionException GatewayReceivable)
getPayload conn log = try $ do
  msg' <- receiveData conn
  case eitherDecode msg' of
    Right msg -> pure msg
    Left  err -> do
      writeChan
        log
        (  "gateway - received parse Error - "
        <> T.pack err
        <> " while decoding "
        <> TE.decodeUtf8 (BL.toStrict msg')
        )
      pure (ParseError (T.pack err))

heartbeat :: Chan GatewaySendable -> Int -> IORef Integer -> IO ()
heartbeat send interval seqKey = do
  threadDelay (1 * 10 ^ (6 :: Int))
  forever $ do
    num <- readIORef seqKey
    writeChan send (Heartbeat num)
    threadDelay (interval * 1000)

-- | What we need to start an event stream
data ConnectionData = ConnData
  { connection    :: Connection
  , connSessionID :: T.Text
  , connAuth      :: Auth
  , connChan      :: Chan (Either GatewayException Event)
  }

startEventStream
  :: ConnectionData
  -> Int
  -> Integer
  -> Chan GatewaySendable
  -> IORef (Maybe UpdateStatusOpts)
  -> Chan T.Text
  -> IO ConnLoopState
startEventStream conndata interval seqN userSend status log = do
  writeChan log "startEventStream"
  seqKey <- newIORef seqN
  let err :: SomeException -> IO ConnLoopState
      err e = do
        writeChan log ("gateway - eventStream error: " <> T.pack (show e))
        ConnReconnect (connAuth conndata) (connSessionID conndata)
          <$> readIORef seqKey
  handle err $ do
    gateSends    <- newChan
    sendingUsers <- newIORef False
    sendsId      <- forkIO $ sendableLoop
      (connection conndata)
      (Sendables userSend gateSends sendingUsers status log)
    heart <- forkIO $ heartbeat gateSends interval seqKey

    finally (eventStream conndata seqKey interval gateSends sendingUsers log)
            (killThread heart >> killThread sendsId)


eventStream
  :: ConnectionData
  -> IORef Integer
  -> Int
  -> Chan GatewaySendable
  -> IORef Bool
  -> Chan T.Text
  -> IO ConnLoopState
eventStream (ConnData conn seshID auth eventChan) seqKey interval send userSends log
  = loop
 where
  loop :: IO ConnLoopState
  loop = do
    eitherPayload <- getPayloadTimeout conn interval log
    case eitherPayload :: Either ConnectionException GatewayReceivable of
      Left (CloseRequest code str) -> case code of
          -- see Discord and MDN documentation on gateway close event codes
          -- https://discord.com/developers/docs/topics/opcodes-and-status-codes#gateway-gateway-close-event-codes
          -- https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent#properties
        1000 -> ConnReconnect auth seshID <$> readIORef seqKey
        1001 -> ConnReconnect auth seshID <$> readIORef seqKey
        4000 -> ConnReconnect auth seshID <$> readIORef seqKey
        4006 -> pure ConnStart
        4007 -> ConnReconnect auth seshID <$> readIORef seqKey
        4014 -> ConnReconnect auth seshID <$> readIORef seqKey
        _    -> do
          writeChan
            eventChan
            (Left
              (GatewayExceptionConnection (CloseRequest code str)
                                          "Normal event loop close request"
              )
            )
          pure ConnClosed
      Left _ -> ConnReconnect auth seshID <$> readIORef seqKey
      Right (Dispatch event sq) -> do
        writeIORef seqKey sq
        writeChan eventChan (Right event)
        writeIORef userSends True
        loop
      Right (HeartbeatRequest sq) -> do
        writeIORef seqKey sq
        writeChan send (Heartbeat sq)
        loop
      Right (Reconnect) -> ConnReconnect auth seshID <$> readIORef seqKey
      Right (InvalidSession retry) -> if retry
        then ConnReconnect auth seshID <$> readIORef seqKey
        else pure ConnStart
      Right (HeartbeatAck) -> loop
      Right (Hello e     ) -> do
        writeChan
          eventChan
          (Left (GatewayExceptionUnexpected (Hello e) "Normal event loop"))
        pure ConnClosed
      Right (ParseError e) -> do
        writeChan
          eventChan
          (Left (GatewayExceptionEventParseError e "Normal event loop"))
        pure ConnClosed

data Sendables = Sendables
  {
  -- | Things the user wants to send. Doesn't reset on reconnect
    sendchan         :: Chan GatewaySendable
  -- | Things the library needs to send. Resets to empty on reconnect
  , gatewaySends     :: Chan GatewaySendable
  -- | If we're really authenticated yet
  , startSendingUser :: IORef Bool
  -- | the last sent status
  , sendslastStatus  :: IORef (Maybe UpdateStatusOpts)
  -- | Log
  , sendlog          :: Chan T.Text
  }

-- simple idea: send payloads from user/sys to connection
-- has to be complicated though
sendableLoop :: Connection -> Sendables -> IO ()
sendableLoop conn sends = sendSysLoop
 where
  sendSysLoop = do
    threadDelay $ round ((10 ^ (6 :: Int)) * (62 / 120) :: Double)
    payload <- readChan (gatewaySends sends)
    sendTextData conn (encode payload)
    usersending <- readIORef (startSendingUser sends)
    if not usersending
      then sendSysLoop
      else do
        act <- readIORef (sendslastStatus sends)
        case act of
          Nothing   -> pure ()
          Just opts -> sendTextData conn (encode (UpdateStatus opts))
        sendUserLoop

  sendUserLoop = do
      -- send a ~120 events a min by delaying
    threadDelay $ round ((10 ^ (6 :: Int)) * (62 / 120) :: Double)
    let e :: Either GatewaySendable GatewaySendable -> GatewaySendable
        e = either id id
    payload <- e
      <$> race (readChan (sendchan sends)) (readChan (gatewaySends sends))
    sendTextData conn (encode payload)
    -- writeChan (sendlog sends) ("extrainfo - sending " <> T.pack (show payload))

    case payload of
      UpdateStatus opts -> writeIORef (sendslastStatus sends) (Just opts)
      _                 -> pure ()

    sendUserLoop
