{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Discord.Test.Random where

import           Control.Monad
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( intercalate )
import           Data.Text                     as T
                                         hiding ( intercalate
                                                , length
                                                , map
                                                , unwords
                                                )
import           Data.Text.Encoding            as E
import           Data.Word
import           Discord.Internal.Types
import           System.Random.Stateful

newVoiceReadyState :: (StatefulGen g m) => g -> m VoiceReadyState
newVoiceReadyState rng = do
  ssrc <- newInteger rng
  ip   <- newIPv4 rng
  port <- newInteger rng
  mode <- newVoiceMode rng
  pure $ VoiceReadyState ssrc ip port [mode]

newText :: (StatefulGen g m) => g -> m T.Text
newText rng =
  (uniformRM (1 :: Int, 64 :: Int) rng >>= (`uniformByteStringM` rng))
    <&> E.decodeUtf8

-- TODO: fromIntegral cast `Int` to `Integer`.
newInteger :: forall g m . (StatefulGen g m) => g -> m Integer
newInteger rng = (uniformM rng :: m Int) <&> fromIntegral

newVoiceMode :: (StatefulGen g m) => g -> m VoiceMode
newVoiceMode rng = uniformRM (0, nrOfVoiceModes-1) rng <&> (voicemodes !!)
 where
  voicemodes =
    [XSALSA20_POLY1305, XSALSA20_POLY1305_SUFFIX, XSALSA20_POLY1305_LITE]
  nrOfVoiceModes = length voicemodes

newIPv4 :: forall g m . (StatefulGen g m) => g -> m T.Text
newIPv4 rng = do
  ns <- replicateM 4 (uniformRM (0, 255) rng) :: m [Word8]
  pure . T.pack . intercalate "." . map show $ ns
