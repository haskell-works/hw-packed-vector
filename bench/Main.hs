{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.List
import Data.Word
import System.Directory

import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.AtIndex           as HW
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified HaskellWorks.Data.PackedVector      as PV


makeBenchW64s :: IO [Benchmark]
makeBenchW64s = do
  entries <- listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".word64s" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (IO.mmapFromForeignRegion file) $ \(v :: DVS.Vector Word64) -> bgroup "with Vector64" $ mempty
      <> [bench ("PV.fromList"  <> file) (whnf (PV.fromList                10) (DVS.toList v))]
      <> [bench ("PV.fromListN" <> file) (whnf (PV.fromListN (HW.length v) 10) (DVS.toList v))]
    , env (fmap (PV.fromList 10 . DVS.toList) (IO.mmapFromForeignRegion file)) $ \pv -> bgroup "with PackedVector64" $ mempty
      <> [bench ("PV.atIndex "  <> file) (whnf (HW.atIndex pv) 20)]
    ]
  return (join benchmarks)

main :: IO ()
main = do
  benchmarks <- (mconcat <$>) $ sequence $ mempty
    <> [makeBenchW64s]
  defaultMain benchmarks
