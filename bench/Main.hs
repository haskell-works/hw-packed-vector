{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.List
import Data.Monoid      ((<>))
import Data.Word
import System.Directory

import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO

makeBenchW64s :: IO [Benchmark]
makeBenchW64s = do
  entries <- listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".rsbs" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (IO.mmapFromForeignRegion file) $ \(v :: DVS.Vector Word64) -> bgroup "blah" $ mempty
      <> [bench ("DVS.size" <> file) (whnf DVS.length v)]
    ]
  return (join benchmarks)

main :: IO ()
main = do
  benchmarks <- (mconcat <$>) $ sequence $ mempty
    <> [makeBenchW64s]
  defaultMain benchmarks
