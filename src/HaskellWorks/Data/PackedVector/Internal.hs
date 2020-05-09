module HaskellWorks.Data.PackedVector.Internal
  ( packBits
  , packBits'
  , unpackBits
  , unpackBits'
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.FixedBitSize
import HaskellWorks.Data.Bits.LoBitsSized
import HaskellWorks.Data.Positioning

{- HLINT ignore "Reduce duplication"  -}

class Integral a => PackBits a where
  packBits :: Count -> [a] -> [a]
  packBits = packBits' 0 0

  packBits' :: Count -> a -> Count -> [a] -> [a]

class Integral a => UnpackBits a where
  unpackBits :: Int -> Count -> [a] -> [a]
  unpackBits = unpackBits' 0 0

  unpackBits' :: Count -> a -> Int -> Count -> [a] -> [a]

instance PackBits Word64 where
  packBits' filled carry bitLen (w:ws) = if fillNeeded < fromIntegral (fixedBitSize carry)
      then packBits' fillNeeded newV bitLen ws
      else newV : packBits' fillLeft carryV bitLen ws
    where fillNeeded  = filled + bitLen
          fillMet     = fillNeeded `min` fromIntegral (fixedBitSize carry)
          fillLeft    = fillNeeded - fillMet
          bitMet      = fromIntegral (fillMet - filled) :: Count
          newV        = carry .|. ((w .&. loBitsSized bitMet) .<. fromIntegral filled)
          carryV      = w .>. bitMet
  packBits' _ carry _ _ = [carry]

instance UnpackBits Word64 where
  unpackBits' _ _ 0 _ _ = []
  unpackBits' filled carry dataLen bitLen ws | filled >= bitLen =
    let result = (carry .&. loBitsSized bitLen) : unpackBits' (filled - bitLen) (carry .>. fromIntegral bitLen) (dataLen - 1) bitLen ws in
    result
  unpackBits' filled carry dataLen bitLen (w:ws) =
    let bitsNeeded = bitLen - filled                    in
    let newValue = carry .|. ((w .&. loBitsSized bitsNeeded) .<. fromIntegral filled) in
    newValue : unpackBits' (fromIntegral (fixedBitSize carry) - bitsNeeded) (w .>. fromIntegral bitsNeeded) (dataLen - 1) bitLen ws
  unpackBits' _ _ _ _ _ = []

instance PackBits Word8 where
  packBits' filled carry bitLen (w:ws) = if fillNeeded < fromIntegral (fixedBitSize carry)
      then packBits' fillNeeded newV bitLen ws
      else newV : packBits' fillLeft carryV bitLen ws
    where fillNeeded  = filled + bitLen
          fillMet     = fillNeeded `min` fromIntegral (fixedBitSize carry)
          fillLeft    = fillNeeded - fillMet
          bitMet      = fromIntegral (fillMet - filled) :: Count
          newV        = carry .|. ((w .&. loBitsSized bitMet) .<. fromIntegral filled)
          carryV      = w .>. fromIntegral bitMet
  packBits' _ carry _ _ = [carry]

instance UnpackBits Word8 where
  unpackBits' _ _ 0 _ _ = []
  unpackBits' filled carry dataLen bitLen ws | filled >= bitLen =
    (carry .&. loBitsSized bitLen) : unpackBits' (filled - bitLen) (carry .>. fromIntegral bitLen) (dataLen - 1) bitLen ws
  unpackBits' filled carry dataLen bitLen (w:ws) =
    let bitsNeeded = bitLen - filled                    in
    let newValue = carry .|. ((w .&. loBitsSized bitsNeeded) .<. fromIntegral filled) in
    let result = newValue : unpackBits' (8 - bitsNeeded) (w .>. fromIntegral bitsNeeded) (dataLen - 1) bitLen ws in
    result
  unpackBits' _ _ _ _ _ = []
