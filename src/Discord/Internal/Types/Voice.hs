{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Discord.Internal.Types.Voice where

import           Data.Aeson
import           Data.Aeson.Types
import Data.Functor ((<&>))
import           Data.Char
import           Data.Text                     as T
                                         hiding ( toLower )
import           Data.Time.Clock                ( UTCTime )
import           Discord.Internal.Types.Guild
import           Discord.Internal.Types.Prelude

data VoiceConnectionSendable
    = VoiceIdentify VoiceIdentifyState
    | VoiceSelectProtocol
    | VoiceHeartbeat
    | VoiceSpeakingOut
    | VoiceResume
    deriving (Show)

data VoiceConnectionReceivable
    = VoiceReady VoiceReadyState
    | VoiceSessionDescription
    | VoiceSpeakingIn
    | VoiceHeartbeatACK
    | VoiceHello Integer
    | VoiceResumed
    | VoiceClientDisconnect
    | ErrParse T.Text
    deriving (Show)

data VoiceConnectionError
    = ErrVoiceInvalidOpcode
    | ErrVoiceInvalidIdentifyPayload
    | ErrVoiceUnauthenticated
    | ErrVoiceAuthenticationFailed
    | ErrVoiceAlreadyAuthenticated
    | ErrVoiceSessionInvalid
    | ErrVoiceSessionTimeout
    | ErrVoiceServerNotFound
    | ErrVoiceInvalidProtocol
    | ErrVoiceDisconnected
    | ErrVoiceServerCrashed
    deriving (Show)

data VoiceIdentifyState = VoiceIdentifyState
  { voiceIdentifyServerID  :: ServerId
  , voiceIdentifyUserID    :: UserId
  , voiceIdentifySessionID :: T.Text
  , voiceIdentifToken      :: T.Text
  }
  deriving Show

data VoiceReadyState = VoiceReadyState
  { voiceReadySSRC  :: Integer
  , voiceReadyIP    :: T.Text
  , voiceReadyPort  :: Integer
  , voiceReadyModes :: VoiceModes
  }
  deriving (Show, Eq)

data VoiceMode = XSALSA20_POLY1305 | XSALSA20_POLY1305_SUFFIX | XSALSA20_POLY1305_LITE
    deriving (Show, Eq)
type VoiceModes = [VoiceMode]

data VoiceState = VoiceState
  { stateGuildId                 :: Maybe GuildId
  , stateChannelId               :: ChannelId
  , stateUserId                  :: UserId
  , stateMember                  :: Maybe GuildMember
  , stateSessionId               :: T.Text
  , stateDeaf                    :: Bool
  , stateMute                    :: Bool
  , stateSelfDeaf                :: Bool
  , stateSelfMute                :: Bool
  , stateSelfStream              :: Maybe Bool
  , stateSelfVideo               :: Bool
  , stateSuppress                :: Bool
  , stateRequestToSpeakTimestamp :: Maybe UTCTime
  }
  deriving (Show, Eq)

data VoiceServer = VoiceServer
  { serverToken    :: T.Text
  , serverGuildId  :: GuildId
  , serverEndpoint :: T.Text
  }
  deriving (Show, Eq)

instance FromJSON VoiceState where
  parseJSON = withObject "VoiceState" $ \o ->
    VoiceState
      <$> o
      .:? "guild_id"
      <*> o
      .:  "channel_id"
      <*> o
      .:  "user_id"
      <*> o
      .:? "member"
      <*> o
      .:  "session_id"
      <*> o
      .:  "deaf"
      <*> o
      .:  "mute"
      <*> o
      .:  "self_deaf"
      <*> o
      .:  "self_mute"
      <*> o
      .:? "self_stream"
      <*> o
      .:  "self_video"
      <*> o
      .:  "suppress"
      <*> o
      .:  "request_to_speak_timestamp"

instance ToJSON VoiceState where
  toJSON VoiceState {..} = object
    [ (name, value)
    | (name, Just value) <-
      [ ("guild_id"                  , toJSON <$> stateGuildId)
      , ("channel_id"                , pure (toJSON stateChannelId))
      , ("user_id"                   , pure (toJSON stateUserId))
      , ("member"                    , toJSON <$> stateMember)
      , ("session_id"                , pure (toJSON stateSessionId))
      , ("deaf"                      , pure (toJSON stateDeaf))
      , ("mute"                      , pure (toJSON stateMute))
      , ("self_deaf"                 , pure (toJSON stateSelfDeaf))
      , ("self_mute"                 , pure (toJSON stateSelfMute))
      , ("self_stream"               , toJSON <$> stateSelfStream)
      , ("self_video"                , pure (toJSON stateSelfVideo))
      , ("suppress"                  , pure (toJSON stateSuppress))
      , ("request_to_speak_timestamp", toJSON <$> stateRequestToSpeakTimestamp)
      ]
    ]

instance FromJSON VoiceServer where
  parseJSON = withObject "VoiceServer" $ \o ->
    VoiceServer <$> o .: "token" <*> o .: "guild_id" <*> o .: "endpoint"

instance ToJSON VoiceServer where
  toJSON VoiceServer {..} = object
    [ (name, value)
    | (name, Just value) <-
      [ ("token"   , pure (toJSON serverToken))
      , ("guild_id", pure (toJSON serverGuildId))
      , ("endpoint", pure (toJSON serverEndpoint))
      ]
    ]

instance FromJSON VoiceConnectionReceivable where
  parseJSON = withObject "payload" $ \o -> do
    op <- o .: "op" :: Parser Int
    case op of
      2  -> VoiceReady <$> o .: "d"
      4  -> fail ("session description not implemented: " <> show o)
      5  -> fail ("voice speaking in not implemented: " <> show o)
      6  -> fail ("heartbeat ack not implemented: " <> show o)
      8  -> (o .: "d" >>= (.: "heartbeat_interval")) <&> VoiceHello
      9  -> fail ("voice resumed not implemented: " <> show o)
      13 -> fail ("voice client disconnected not implemented: " <> show o)
      _  -> fail ("Unknown Receivable payload ID:" <> show op)

instance FromJSON VoiceReadyState where
  parseJSON = withObject "VoiceReadyState" $ \o ->
    VoiceReadyState
      <$> o
      .:  "ssrc"
      <*> o
      .:  "ip"
      <*> o
      .:  "port"
      <*> ((o .: "modes") >>= parseJSONList >>= \a ->
            return $ Prelude.map mkVoiceMode a
          )

instance ToJSON VoiceReadyState where
  toJSON VoiceReadyState {..} = object
    [ (name, value)
    | (name, Just value) <-
      [ ("ssrc" , pure (toJSON voiceReadySSRC))
      , ("ip"   , pure (toJSON voiceReadyIP))
      , ("port" , pure (toJSON voiceReadyPort))
      , ("modes", pure (toJSON voiceReadyModes))
      ]
    ]

mkVoiceMode :: String -> VoiceMode
mkVoiceMode "xsalsa20_poly1305"        = XSALSA20_POLY1305
mkVoiceMode "xsalsa20_poly1305_suffix" = XSALSA20_POLY1305_SUFFIX
mkVoiceMode "xsalsa20_poly1305_lite"   = XSALSA20_POLY1305_LITE
mkVoiceMode m = error ("unsupported voicemode given: " <> show m)

instance ToJSON VoiceMode where
  toJSON = toJSON . Prelude.map toLower . show
