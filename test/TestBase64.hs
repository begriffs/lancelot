{-# LANGUAGE TemplateHaskell #-}

module TestBase64 where

import Test.QuickCheck.All
import Test.QuickCheck.Instances ()

import qualified Data.ByteString as BS
import Codec.Binary.Base64 (encode)

prop_lengthMultipleOf4 :: BS.ByteString -> Bool
prop_lengthMultipleOf4 = \s ->
  BS.length (encode s) `rem` 4 == 0

----------------------------------------------------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
