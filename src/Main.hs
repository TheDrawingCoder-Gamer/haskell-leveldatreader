{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bulby.DatReader.NBTData
import Bulby.DatReader.Region
import qualified Data.ByteString.Lazy as BL 
import Data.Binary 
import System.Environment (getArgs) 
import qualified Data.Aeson as A
import Data.Maybe (fromJust)
main :: IO ()
main = do
  args <- getArgs 
  thingie <- BL.readFile (head args) 
  let tag = Region thingie Nothing  
  let chunk = fromJust $ getChunkFromRegion tag 1
  let blocks = blocksOfChunk chunk 
  print blocks
  A.encodeFile (head args ++ ".json") (NBTCompound (JavaText "") (chunkData chunk))
