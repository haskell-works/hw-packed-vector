module HaskellWorks.Data.PackedVector.IO
  ( -- writeAsPackedVector
  ) where

-- import Data.Word

-- import qualified Data.ByteString.Builder as B
-- import qualified Data.ByteString.Lazy    as LBS
-- import qualified Data.Vector.Storable    as DVS
-- import qualified System.IO               as IO

-- writeVector32AsPackedVector :: IO.Handle -> Int -> DVS.Vector Word32 -> IO Int
-- writeVector32AsPackedVector hOut bitSize v = go 0 0
--   where go :: Int -> IO Int
--         go written n | n >= DVS.length v = return written
