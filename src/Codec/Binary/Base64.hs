module Codec.Binary.Base64 (encode) where

import Control.Applicative
import Control.Monad
import qualified Data.Binary.Get as BY
import qualified Data.Binary.Bits.Get as BI
import Data.Bits (shiftL)
import qualified Data.ByteString as BL
import Data.Monoid ((<>))
import Data.Word

encode =
  BY.runGet getB64
 where
  getB64 = do
    slices <- many $
          (Slice <$> takeGroups [grp 4 6 0]            <*> return 0)
      <|> (Slice <$> takeGroups [grp 2 6 0, grp 1 4 2] <*> return 1)
      <|> (Slice <$> takeGroups [grp 1 6 0, grp 1 2 4] <*> return 2)
    return $ BL.concat (map encodeSlice slices)

  takeGroups :: [BI.BitGet [a]] -> BY.Get [a]
  takeGroups = fmap concat . BI.runBitGet . sequence

  grp :: Int -> Int -> Int -> BI.BitGet [Word8]
  grp n sz sh = replicateM n $ flip shiftL sh <$> BI.getWord8 sz

  encodeSlice :: Slice -> BL.ByteString
  encodeSlice (Slice bs p) = encBs <> padding
   where
    encBs = BL.pack . map encodeWord $ bs
    padding = BL.pack $ replicate p 61

    encodeWord w
      | w < 26 = w      + 65  -- A-Z
      | w < 52 = (w-26) + 97  -- a-z
      | w < 62 = (w-52) + 48  -- 0-9
      | w == 62 = 43           -- +
      | otherwise = 47        -- /

data Slice = Slice [Word8] Int
