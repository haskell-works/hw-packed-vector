{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Semigroup                 ((<>))
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import Options.Applicative            hiding (columns)

import qualified App.Commands.Types      as Z
import qualified Data.Binary.Get         as G
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LBS
import qualified System.Exit             as IO
import qualified System.IO               as IO

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

    IO.withBinaryFile (opts ^. the @"output") IO.WriteMode $ \hOut -> do
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
