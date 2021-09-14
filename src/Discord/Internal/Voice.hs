module Discord.Internal.Voice where

import           Control.Concurrent             ( ThreadId
                                                , forkIO
                                                )
import           Control.Concurrent.Chan        ( Chan
                                                , newChan
                                                )
import           Data.IORef                     ( newIORef )
import           Data.Text                     as T
import           Prelude                 hiding ( log )

import           Discord.Internal.Gateway.EventLoop
                                                ( DiscordHandleGateway )
import           Discord.Internal.Types
import           Discord.Internal.Voice.EventLoop

startVoiceConnThread
  :: URL -> Auth -> Chan T.Text -> IO (DiscordHandleGateway, ThreadId)
startVoiceConnThread url auth log = do
  sends  <- newChan
  status <- newIORef Nothing
  tid    <- forkIO $ connectionLoop url auth (undefined, sends, status) log
  pure ((undefined, sends, status), tid)

-- Connecting to Voice:
--
--  1. [X]  Formulate `Opcode 4 Gateway Voice State Update` request establish voice
--          connectivity.
--
--  2. [X]  On success:
--     [X]  2.1 Receive `Voice State Update`.
--              - Contains `session_id`.
--              ```json
--              {
--                "channel_id": "157733188964188161",
--                "user_id": "80351110224678912",
--                "session_id": "90326bd25d71d39b9ef95b299e3872ff",
--                "deaf": false,
--                "mute": false,
--                "self_deaf": false,
--                "self_mute": true,
--                "suppress": false,
--                "request_to_speak_timestamp": "2021-03-31T18:45:31.297561+00:00"
--              }
--              ```
--     [X]  2.2 Receive `Voice Server Update`.
--              - Contains server information to establish new voice connection.
--
--               Voice Server Update:
--               ```json
--               {
--                "t": "VOICE_SERVER_UPDATE",
--                "s": 2,
--                "op": 0,
--                "d": {
--                  "token": "my_token",
--                  "guild_id": "41771983423143937",
--                  "endpoint": "smart.loyal.discord.gg"
--                }
--               }
--               ```
--               - The `endpoint` is a `wss://` endpoint, might have to handle that.
--
--  3. [ ]  Connect to endpoint:
--           3.1 Send `Voice Identify Payload`.
--               ```json
--               {
--                 "op": 0,
--                 "d": {
--                   "server_id": "41771983423143937",
--                   "user_id": "104694319306248192",
--                   "session_id": "my_session_id",
--                   "token": "my_token"
--                 }
--               }
--               ```
--           3.2 Receive `Voice Ready Payload`.
--           ```json
--           {
--             "op": 2,
--             "d": {
--                 "ssrc": 1,
--                 "ip": "127.0.0.1",
--                 "port": 1234,
--                 "modes": ["xsalsa20_poly1305", "xsalsa20_poly1305_suffix", "xsalsa20_poly1305_lite"],
--                 "heartbeat_interval": 1 <-- Ignore.
--             }
--           }
--           ```
--           3.3 Handle `Hello Payload`.
--               - The hello payload contains the heartbeat interval
--               ```json
--               {
--                 "op": 8,
--                 "d": {
--                   "heartbeat_interval": 41250 <-- In ms.
--                 }
--               }
--               ```
--               - Spawn a thread continously updating the heartbeat and acknowledging.
--
--  4.      Create the UDP connection over the created websocket.
--           4.1 Do IP-Discovery to emit own endpoint to discord server.
--           4.2 Use own endpoint data and send a `Select Protocol Payload`:
--           ```json
--           {
--             "op": 1,
--             "d": {
--                 "protocol": "udp",
--                 "data": {
--                     "address": "127.0.0.1",
--                     "port": 1337,
--                     "mode": "xsalsa20_poly1305_lite"
--                 }
--             }
--           }
--           ```
--           4.3 Receive `Session Description` including the secret_key and mode:
--           ```json
--           {
--               "op": 4,
--               "d": {
--                   "mode": "xsalsa20_poly1305_lite",
--                   "secret_key": [ ...251, 100, 11...]
--               }
--           }
--           ```
--
--  5.      Encrypt and send voice data.
--           5.1 Encode voice data in `OPUS`.
--               - 2-channel with 48kHz.
--           5.2 Encrypt `OPUS` audio data using the secret key from 4.3 & libsodium.
--           5.3 Create payload and send:
--               - | RTP HEADER | ENCRYPTED AUDIO | NONCE |
--           5.4 Optionally indicate speaking started/stopped.
--
-- 6.       Handle edge cases of reconnecting and whatever else comes up.
