module HaskellWorks.Data.PackedVector.PackedVector64Spec (spec) where

import HaskellWorks.Data.AtIndex                     ((!!!))
import HaskellWorks.Data.PackedVector.PackedVector64
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.AtIndex                     as HW
import qualified HaskellWorks.Data.PackedVector.PackedVector64 as PV
import qualified HaskellWorks.Gen                              as G
import qualified Hedgehog.Gen                                  as G
import qualified Hedgehog.Range                                as R

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.PackedVector.PackedVector64Spec" $ do
  it "fromList/toList works" $ require $ property $ do
    wSize <- forAll $ G.subWordSize 64
    len   <- forAll $ G.int   (R.linear 1 128)
    ws    <- forAll $ G.list  (R.singleton len) (G.word64OfSize wSize)
    toList (fromList wSize ws) === ws
  it "fromListN/toList works" $ require $ property $ do
    wSize <- forAll $ G.subWordSize 64
    len   <- forAll $ G.int   (R.linear 1 128)
    ws    <- forAll $ G.list  (R.singleton len) (G.word64OfSize wSize)
    toList (fromListN (HW.length ws) wSize ws) === ws
  it "Index lookup works" $ require $ property $ do
    wSize <- forAll $ G.subWordSize 64
    len   <- forAll $ G.int   (R.linear 1 32)
    ws    <- forAll $ G.list  (R.singleton len) (G.word64OfSize wSize)
    i     <- forAll $ G.int64 (R.linear 0 (fromIntegral len - 1))
    (PV.fromList wSize ws !!! i) === (ws !!! i)
