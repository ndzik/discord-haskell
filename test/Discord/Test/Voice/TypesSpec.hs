module Discord.Test.Voice.TypesSpec
  ( spec
  ) where

import           Data.Time.Clock.POSIX
import           Discord.Test.Random
import           Generic.Encoding
import           System.Random
import           System.Random.Stateful
import           Test.Hspec

spec :: Spec
spec = do
  describe "voice types" $ do
    it "de-/encode VoiceConnectionReceivables" $ do
      rng <- getPOSIXTime >>= (newIOGenM . mkStdGen) . round
      newVoiceReadyState rng >>= genericJSONEncodingDecodingTest
