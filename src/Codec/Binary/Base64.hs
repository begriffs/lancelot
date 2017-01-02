module Codec.Binary.Base64 where

import Control.Applicative
import Control.Monad
import qualified Data.Binary.Get as BY
import qualified Data.Binary.Bits.Get as BI
import qualified Data.ByteString as BL
import Data.Char
import Data.Word

import Prelude hiding (many)

encode = BY.runGet getB64
 where
  getB64 = do
    slices <- many $ threeByter <|> twoByter <|> oneByter
    return $ BL.concat (map encodeSlice slices)

  threeByter :: BY.Get [Word8]
  threeByter = BI.runBitGet . replicateM 4 $ BI.getWord8 6

  twoByter = undefined
  oneByter = undefined

  encodeSlice :: [Word8] -> BL.ByteString
  encodeSlice = BL.pack . map encodeWord
   where
    encodeWord w
      | w < 26 = w      + 65  -- A-Z
      | w < 52 = (w-26) + 96  -- a-z
      | w < 62 = (w-52) + 48  -- 0-9
      | w == 62 = 43           -- +
      | otherwise = 47        -- /
