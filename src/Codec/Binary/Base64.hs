module Codec.Binary.Base64 where

import Control.Applicative
import Control.Monad
import qualified Data.Binary.Get as BY
import qualified Data.Binary.Bits.Get as BI
import qualified Data.ByteString as BL
import Data.Word

encode =
  BY.runGet getB64
 where
  getB64 = do
    slices <- many $
          takeGroups [4 `x` 6]
      <|> takeGroups [2 `x` 6, 1 `x` 4]
      <|> takeGroups [1 `x` 6, 1 `x` 2]
    return $ BL.concat (map encodeSlice slices)

  takeGroups :: [BI.BitGet [a]] -> BY.Get [a]
  takeGroups = fmap concat . BI.runBitGet . sequence

  x :: Int -> Int -> BI.BitGet [Word8]
  x n sz = replicateM n $ BI.getWord8 sz

  padding :: Int -> BI.BitGet [Word8]
  padding n = return $ replicate n 61 -- =

  encodeSlice :: [Word8] -> BL.ByteString
  encodeSlice = BL.pack . map encodeWord
   where
    encodeWord w
      | w < 26 = w      + 65  -- A-Z
      | w < 52 = (w-26) + 96  -- a-z
      | w < 62 = (w-52) + 48  -- 0-9
      | w == 62 = 43           -- +
      | otherwise = 47        -- /
