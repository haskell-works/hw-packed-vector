module HaskellWorks.Data.PackedVector.InternalSpec (spec) where

import HaskellWorks.Data.PackedVector.Internal
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Gen as G
import qualified Hedgehog.Gen     as G
import qualified Hedgehog.Range   as R

{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.PackedVector.InternalSpec" $ do
  it "PackedVector Word8" $ require $ property $ do
    wSize <- forAll $ G.subWordSize 8
    len   <- forAll $ G.int   (R.linear 1 3)
    ws    <- forAll $ G.list  (R.singleton len) (G.word8OfSize wSize)
    unpackBits (length ws) wSize (packBits wSize ws) === ws
  it "PackedVector Word64" $ require $ property $ do
    wSize <- forAll $ G.subWordSize 64
    len   <- forAll $ G.int   (R.linear 1 128)
    ws    <- forAll $ G.list  (R.singleton len) (G.word64OfSize wSize)
    unpackBits (length ws) wSize (packBits wSize ws) === ws
