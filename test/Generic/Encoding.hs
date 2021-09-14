module Generic.Encoding where

import           Data.Aeson
import           Test.Hspec

genericJSONEncodingDecodingTest
  :: (Show a, Eq a, FromJSON a, ToJSON a) => a -> Expectation
genericJSONEncodingDecodingTest msg = case decode . encode $ msg of
  Nothing  -> error "unable to de-/encode msg type"
  Just got -> got `shouldBe` msg
