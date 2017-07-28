module HaskellWorks.Data.PackedVector.PackedVector64Spec (spec) where

import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.PackedVector.PackedVector64
import Hedgehog
import Test.Hspec
-- import Test.QuickCheck
import HaskellWorks.Hspec.Hedgehog

import qualified HaskellWorks.Gen          as G
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.PackedVector.PackedVector64Spec" $ do
  it "Round trip from list" $ require $ property $ do
    wSize <- forAll $ G.subWordSize 64
    len   <- forAll $ G.int   (R.linear 1 128)
    ws    <- forAll $ G.list  (R.singleton len) (G.word64OfSize wSize)
    toList (fromList wSize ws) === ws
  it "Round trip from list" $ require $ property $ do
    wSize <- forAll $ G.subWordSize 64
    len   <- forAll $ G.int   (R.linear 1 32)
    ws    <- forAll $ G.list  (R.singleton len) (G.word64OfSize wSize)
    i     <- forAll $ G.int64 (R.linear 0 (fromIntegral len - 1))
    (fromList wSize ws !!! i) === (ws !!! i)
