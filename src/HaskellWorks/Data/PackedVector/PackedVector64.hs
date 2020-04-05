{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module HaskellWorks.Data.PackedVector.PackedVector64
  ( PackedVector64(..)
  , empty
  , fromList
  , fromListN
  , toList
  , createFileIndex
  ) where

import Control.DeepSeq
import Data.Int
import Data.Word
import GHC.Generics
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.LoBitsSized
import HaskellWorks.Data.PackedVector.Internal
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Unsign
import Prelude                                 hiding (length)

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Vector.Storable    as DVS
import qualified System.IO               as IO

data PackedVector64 = PackedVector64
  { swBuffer    :: !(DVS.Vector Word64)
  , swBitSize   :: !Word
  , swBufferLen :: !Int
  } deriving (Eq, Show, Generic)

instance NFData PackedVector64

empty :: PackedVector64
empty =
  PackedVector64
  { swBuffer    = DVS.empty
  , swBufferLen = 0
  , swBitSize   = 1
  }

instance Container PackedVector64 where
  type Elem PackedVector64 = Word64

instance Length PackedVector64 where
  length = fromIntegral . swBufferLen
  {-# INLINE length #-}

instance AtIndex PackedVector64 where
  atIndex v i =
    let bitSize     = fromIntegral (swBitSize v) :: Count
        bitIndex    = fromIntegral (swBitSize v) * i
        (q, r)      = bitIndex `quotRem` 64
        vv          = swBuffer v
    in if r <= 64 - fromIntegral bitSize
      then -- Not crossing boundary
        ((vv !!! q) .>. unsign r) .&. loBitsSized bitSize
      else -- Crossing boundary
        let loBitsSize  = 64 - toCount r
            hiBitsSize  = bitSize - loBitsSize
            loBits      = ((vv !!! q) .>. unsign r) .&. loBitsSized loBitsSize
            hiBits      = (vv !!! (q + 1)) .&. loBitsSized hiBitsSize
        in  loBits .|. (hiBits .<. loBitsSize)
  (!!!)       = atIndex
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

fromList :: Count -> [Word64] -> PackedVector64
fromList wordLength ws = PackedVector64
  { swBuffer    = DVS.fromList (packBits wordLength ws)
  , swBufferLen = fromIntegral (length ws)
  , swBitSize   = fromIntegral wordLength
  }

fromListN :: Count -> Count -> [Word64] -> PackedVector64
fromListN vectorSize wordLength ws = PackedVector64
  { swBuffer    = DVS.fromListN (fromIntegral vectorSize) (packBits wordLength ws)
  , swBufferLen = fromIntegral (length ws)
  , swBitSize   = fromIntegral wordLength
  }

toList :: PackedVector64 -> [Word64]
toList v = unpackBits (swBufferLen v) (fromIntegral (swBitSize v)) (DVS.toList (swBuffer v))

encodePacked :: Count -> [Word64] -> B.Builder
encodePacked wordSize = go 0 0
  where go :: Count -> Word64 -> [Word64] -> B.Builder
        go 0           _   [] = mempty
        go _           acc [] = B.word64LE acc
        go bitsWritten acc (w:ws) =
          let totalBits   = bitsWritten + wordSize
              excessBits  = totalBits - 64
              newAcc      = (w .<. bitsWritten) .|. acc
          in if totalBits >= 64
            then B.word64LE newAcc <>             go excessBits (w .>. (wordSize - excessBits))     ws
            else let freeBits = 64 - totalBits in go totalBits  (newAcc .<. freeBits .>. freeBits)  ws

createFileIndex :: IO.Handle -> Count -> Count -> [Word64] -> IO ()
createFileIndex hOut wordSize inSize ws = do
  headerPos <- IO.hTell hOut

  B.hPutBuilder hOut $ mempty
    <> B.word64LE wordSize              -- Number of bits in a packed word
    <> B.word64LE (fromIntegral inSize) -- Number of entries
    <> B.word64LE 0                     -- Number of bytes in packed vector

  startPos <- IO.hTell hOut

  -- TODO Write packed vector instead
  LBS.hPut hOut (B.toLazyByteString (encodePacked wordSize ws))

  endPos <- IO.hTell hOut

  let vBytes = endPos - startPos

  IO.hSeek hOut IO.AbsoluteSeek headerPos

  B.hPutBuilder hOut $ mempty
    <> B.word64LE wordSize              -- Number of bits in a packed word
    <> B.word64LE (fromIntegral inSize) -- Number of entries
    <> B.word64LE (fromIntegral vBytes) -- Number of bytes in packed vector

  IO.hSeek hOut IO.AbsoluteSeek endPos
