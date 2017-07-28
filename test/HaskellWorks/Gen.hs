module HaskellWorks.Gen where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import Hedgehog

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

subWordSize :: MonadGen m => Count -> m Count
subWordSize maxWordSize = G.word64 (R.linear 1 maxWordSize)

word8OfSize :: MonadGen m => Count -> m Word8
word8OfSize sz = G.word8 (R.linear 0 (1 .<. fromIntegral sz - 1))

word64OfSize :: MonadGen m => Count -> m Word64
word64OfSize sz = G.word64 (R.linear 0 (1 .<. fromIntegral sz - 1))
