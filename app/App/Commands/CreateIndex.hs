{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Semigroup            ((<>))
import Data.Word
import Options.Applicative       hiding (columns)

import qualified App.Commands.Types                            as Z
import qualified Data.Binary.Get                               as G
import qualified Data.ByteString.Lazy                          as LBS
import qualified HaskellWorks.Data.PackedVector.PackedVector64 as PV
import qualified System.Exit                                   as IO
import qualified System.IO                                     as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

decodeWord32s :: LBS.ByteString -> [Word32]
decodeWord32s = fmap (G.runGet G.getWord32le) . go
  where go :: LBS.ByteString -> [LBS.ByteString]
        go lbs = case LBS.splitAt 4 lbs of
          (lt, rt) -> if LBS.length lt == 4
            then lt:go rt
            else if LBS.length lt == 0
              then []
              else [LBS.take 4 (lt <> LBS.replicate 4 0)]

runCreateIndex :: Z.CreateIndexOptions -> IO ()
runCreateIndex opts = do
  let wordSize = opts ^. the @"wordSize"

  IO.withBinaryFile (opts ^. the @"input") IO.ReadMode $ \hIn -> do
    inSize <- IO.hFileSize hIn

    when (inSize `mod` 4 /= 0) $ do
      IO.hPutStrLn IO.stderr "Input file should have size multiple of 4-bytes"
      IO.exitFailure

    lbs <- LBS.hGetContents hIn

    let ws = fmap fromIntegral (decodeWord32s lbs) :: [Word64]

    PV.createFileIndex (opts ^. the @"output") wordSize (fromIntegral inSize) ws

optsCreateIndex :: Parser Z.CreateIndexOptions
optsCreateIndex = Z.CreateIndexOptions
  <$> option auto
        (   long "word-size"
        <>  short 'n'
        <>  help "Size of each word in the packed vector.  Must be between 1 and 64"
        <>  metavar "BIT_COUNT"
        )
  <*> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input file of integers to pack"
        <>  metavar "FILE"
        )
  <*> strOption
        (   long "output"
        <>  short 'o'
        <>  help "Output file"
        <>  metavar "FILE"
        )

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
