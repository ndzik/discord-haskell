{-# LANGUAGE OverloadedStrings #-}

-- | Provides logic code for interacting with the Discord websocket
--   voice server.
module Discord.Internal.Voice.EventLoop where

import           Control.Concurrent.Chan
import           Control.Monad
import           Data.Aeson
import           Data.IORef
import           Data.Text                     as T
import           Network.Socket                 ( HostName
                                                , PortNumber
                                                )
import           Network.WebSockets             ( Connection
                                                , ConnectionException(..)
                                                , receiveData
                                                , sendTextData
                                                )
import           Prelude                 hiding ( log )
import           Wuss                           ( runSecureClient )

import           Control.Exception.Safe         ( SomeException
                                                , finally
                                                , handle
                                                , try
                                                )
import           Discord.Internal.Gateway.EventLoop
                                                ( ConnLoopState(..)
                                                , GatewayException
                                                )
import           Discord.Internal.Types

data URL = URL
  { _hostname :: HostName
  , _port     :: PortNumber
  , _endpoint :: String
  }
  deriving Show

connectVoice :: URL -> (Connection -> IO a) -> IO a
connectVoice (URL hn p ep) = runSecureClient hn p ep

type DiscordHandleVoiceConnection
  = ( Chan (Either GatewayException Event)
    , Chan GatewaySendable
    , IORef (Maybe UpdateStatusOpts)
    )

connectionLoop
  :: URL -> Auth -> DiscordHandleVoiceConnection -> Chan T.Text -> IO ()
connectionLoop url _auth (_events, _userSend, _lastStatus) log = loop
  ConnStart
  0
 where
  loop :: ConnLoopState -> Int -> IO ()
  loop s _retries = case s of
    ConnClosed -> pure ()
    ConnStart  -> do
      next <- try $ connectVoice url handler
      case (next :: Either SomeException ()) of
        Left  msg' -> print "TRIED BUT RIGHT: " >> print msg'
        Right msg' -> print "TRIED BUT LEFT: " >> print msg'
    ConnReconnect{} -> print "tried to reconnect FBM"

  handler :: Connection -> IO ()
  handler conn = do
    msg <- getPayload conn log
    case msg of
      Right msg' -> do
        print "CONNECTION_HANDLER: RIGHT MSG" >> print msg'
      Left msg' -> do
        print "CONNECTION_HANDLER: LEFT MSG" >> print msg'


getPayload
  :: Connection
  -> Chan T.Text
  -> IO (Either ConnectionException VoiceConnectionReceivable)
getPayload conn _ = try $ do
  msg' <- receiveData conn
  case eitherDecode msg' of
    Right msg -> pure msg
    Left  err -> pure (ErrParse (T.pack err))
