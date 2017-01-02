module TestBase64 where

import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (Gen, scale)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Property

import Data.Char (ord)
import Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BL
import Codec.Binary.Base64 (encode)

import Text.Regex.Posix

prop_lengthMultipleOf4 s =
  BL.length (encode s) `rem` 4 == 0

prop_endsWithPadding b =
  collect suffix
    $ (encB =~ ("(^|[^=])" <> suffix <> "$")) -- at end
    && not (encB =~ ("=[^=]"::BL.ByteString)) -- only at end
 where
  encB = encode b
  remainder = fromIntegral $ BL.length b `rem` 3
  suffix = BL.replicate
    ((3 - remainder) `rem` 3)
    (fromIntegral $ ord '=')

prop_outputAlphabet =
  forAll (scale (*3) (arbitrary :: Gen BL.ByteString)) $ \b ->
    let used = S.fromList . BL.unpack $ encode b
        allowed = S.fromList . map (fromIntegral . ord) $
          ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/','='] in
    cover (S.size used == 63) 1 "full-alphabet"
      $ used `S.isSubsetOf` allowed

----------------------------------------------------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
