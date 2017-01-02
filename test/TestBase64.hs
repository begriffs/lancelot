{-# LANGUAGE TemplateHaskell #-}

module TestBase64 where

import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (Gen, scale)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Property

import Data.Char (ord)
import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Codec.Binary.Base64 (encode)

prop_lengthMultipleOf4 s =
  BS.length (encode s) `rem` 4 == 0

prop_endsWithPadding b =
  collect r
    $ suffix `BS.isSuffixOf` encode b
 where
  r = fromIntegral $ BL.length b `rem` 3
  suffix = BS.replicate
    ((3 - r) `rem` 3)
    (fromIntegral $ ord '=')

prop_outputAlphabet =
  forAll (scale (*3) (arbitrary :: Gen BL.ByteString)) $ \b ->
    let used = S.fromList . BS.unpack $ encode b
        allowed = S.fromList . map (fromIntegral . ord) $
          ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/','='] in
    cover (S.size used == 63) 1 "full-alphabet"
      $ used `S.isSubsetOf` allowed

----------------------------------------------------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
