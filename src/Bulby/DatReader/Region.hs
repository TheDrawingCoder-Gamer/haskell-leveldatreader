{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeApplications #-}
module Bulby.DatReader.Region where 

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Builder qualified as BSB
import Data.Binary
import Data.Binary.Get (getWord8, getLazyByteString, getInt32be)
import Data.Binary.Put
import Data.Bits (shiftR, (.&.), rotateR)
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib qualified as ZLib
import Data.Int
import Bulby.DatReader.NBTData
import Data.HashMap.Strict qualified as HM
import Data.Bifunctor (bimap)
import Data.Text qualified as T
import Control.Monad ((=<<))
import Data.Vector qualified as V
import Data.Maybe (mapMaybe, fromMaybe)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M
import Text.Megaparsec.Byte.Binary qualified as M
data Region = Region 
    { regionData :: BL.ByteString 
    , regionEntityData :: Maybe BL.ByteString } 
getChunkFromRegion = getChunkFromRegionWith True 
getChunkFromRegionWith :: Bool -> Region -> Int -> Maybe Chunk 
getChunkFromRegionWith isAnvil region idx = 
    case getChunkStream region idx of 
        Nothing -> 
            Nothing 
        Just (ChunkStream _ c, ec) -> 
            let root = decode c :: NBTTag in 
                case root of 
                    NBTCompound _ compound@(NBTCompoundTag hashmap) -> 
                        let chunkVer = 
                                if isAnvil then 
                                    intTag $ hashmap HM.! "DataVersion"
                                else 
                                    0 
                            xPos = if chunkVer >= 2844 then intTag ( hashmap HM.! "xPos") else intTag (uncompoundTag (compoundTag (hashmap HM.! "Level")) HM.! "xPos")
                            zPos = if chunkVer >= 2844 then intTag ( hashmap HM.! "zPos") else intTag (uncompoundTag (compoundTag (hashmap HM.! "Level")) HM.! "zPos") in 
                        let entityTag = fmap (compoundTag . decode . chunkStream) ec in
                        let sections = fmap (map (uncompoundTag .compoundTag)) (tagListItems . listTag <$> (
                                if chunkVer >= 2844 then 
                                    hashmap HM.!? "sections" 
                                else 
                                    (HM.!? "Sections") . uncompoundTag . compoundTag =<< hashmap HM.!? "Level"
                                ))
                        in
                        let (minY, maxY) = bimap (*16) (\x -> (x + 1) * 16) $
                                if isAnvil then 
                                    case sections of 
                                        Just secs -> 
                                            
                                            let sectionFolder section (minY, maxY) =  
                                                    if HM.member "block_states" section || HM.member "BlockStates" section || HM.member "Blocks" section then 
                                                        let y = fromIntegral (byteTag (section HM.! "Y")) in
                                                        (min minY y, max maxY y)
                                                    else 
                                                        (minY, maxY)
                                            in
                                            -- this only breaks when height is larger than 42069 which shouldn't happen
                                            foldr sectionFolder (69420, -69420) secs 
                                        Nothing -> (0, 256)
                                else 
                                    (0, 256)
                        in 
                        Just $ Chunk compound entityTag (fromIntegral chunkVer) (fromIntegral minY) (fromIntegral maxY) (fromIntegral xPos) (fromIntegral zPos) isAnvil sections
                    _ -> Nothing
getChunkStream :: Region -> Int -> Maybe (ChunkStream, Maybe ChunkStream) 
getChunkStream (Region region entityRegion) idx = 
    let regionChunk = getSingleStream region in 
    case regionChunk of 
        Just c -> 
            case entityRegion of 
                Just r -> 
                    case getSingleStream r of 
                        Just er -> 
                            Just (c, Just er) 
                        Nothing -> 
                            Nothing
                Nothing -> 
                    Just (c, Nothing)
        Nothing -> 
            Nothing
    where 
    getSingleStream :: BL.ByteString -> Maybe ChunkStream 
    getSingleStream regionData = 
        let off = decode (BL.drop (fromIntegral idx * 4) regionData) :: Int32
            sec = off `shiftR` 8 in 
            if sec < 2 then 
                Nothing 
            else 
                let chunkData = BL.drop (fromIntegral sec * 4096) regionData in
                    Just (decode chunkData :: ChunkStream)
data CompressionType
    = GZIP 
    | Inflate
    deriving (Eq, Ord, Enum)

instance Binary CompressionType where 
    get = do 
        byte <- getWord8 
        case byte of 
            1 -> pure GZIP 
            2 -> pure Inflate 
            _ -> fail "Not a valid compression type"
    put kind = putWord8 $ fromIntegral (fromEnum kind + 1)

data ChunkStream = ChunkStream 
    { chunkStreamKind :: CompressionType 
    , chunkStream :: BL.ByteString }

instance Binary ChunkStream where 
    get = do 
        len <- getInt32be 
        kind <- get :: Get CompressionType
        bs <- getLazyByteString (fromIntegral len - 1)
        case kind of 
            GZIP -> 
                pure $ ChunkStream kind (GZip.decompress bs)
            Inflate -> 
                pure $ ChunkStream kind (ZLib.decompress bs)
    put (ChunkStream kind stream) =
        case kind of 
            GZIP -> 
                let compressed = GZip.compress stream 
                    len = BL.length compressed + 1 in 
                    putInt32be (fromIntegral len) <> put kind <> putLazyByteString compressed
            Inflate -> 
                let compressed = ZLib.compress stream 
                    len = BL.length compressed + 1 in 
                    putInt32be (fromIntegral len) <> put kind <> putLazyByteString compressed 

data Chunk = Chunk 
    { chunkData :: NBTCompoundTag
    , chunkEntityData :: Maybe NBTCompoundTag 
    , chunkVer :: Int
    , chunkYMin :: Int 
    , chunkYMax :: Int
    , chunkPosX :: Int 
    , chunkPosZ :: Int 
    , chunkIsAnvil :: Bool
    , chunkSections :: Maybe [HM.HashMap T.Text NBTTag]}

data Identifier = Identifier
    { namespace :: T.Text
    , path :: T.Text }
    deriving Show
identifierOfText :: T.Text -> Identifier 
identifierOfText txt = 
   let txts = T.splitOn ":" txt in 
        case length txts of 
            1 -> Identifier "minecraft" (head txts) 
            2 -> Identifier (head txts) (last txts)
            _ -> error "Identifier can't have more than 1 colon"
data BlockData = BlockData 
    { identifier :: Identifier 
    , blockState :: HM.HashMap T.Text T.Text }
    deriving Show
data Blocks = Blocks 
    { blocksSize :: Int 
    , blocksYMin :: Int 
    , blocksYMax :: Int 
    , blocksBlockData :: V.Vector (Maybe BlockData)
    , blocksBiome :: V.Vector Int32
    , blocksEntities :: [NBTCompoundTag] 
    , blocksTileEntities :: [NBTCompoundTag] }
    deriving Show
getBlockIndex :: Blocks -> Int -> Int -> Int -> Maybe Int 
getBlockIndex (Blocks{blocksYMin=ymin,blocksYMax=ymax}) x y z  
    | x < 0 || x > 15 || z < 0 || z > 15 = Nothing 
    | y < ymin || y >= ymax = Nothing 
    | otherwise = Just $ x + (z * 16) + ((y - ymin) * 16) * 16
getBlockData :: Blocks -> Int -> Int -> Int -> Maybe BlockData 
getBlockData blocks@(Blocks{blocksBlockData=blockData}) x y z = 
    let bIndex = getBlockIndex blocks x y z in
    (blockData V.!) =<< bIndex 
getBlockBiome :: Blocks -> Int -> Int -> Int -> Maybe Int32 
getBlockBiome blocks@(Blocks{blocksBiome=biomes}) x y z = 
    let bIndex = getBlockIndex blocks x y z in 
    (biomes V.!) <$> bIndex
blocksOfChunk :: Chunk -> Maybe Blocks 
blocksOfChunk Chunk{..} = 
    if chunkIsAnvil then 
        case chunkSections of 
            Nothing -> 
                Just $ fastConstruct 0 256 
            Just sections ->
                let blockNum = 16 * 16 * abs (chunkYMax - chunkYMin) in
                let chunkBlocks =  mapMaybe (\section -> 
                        let y = fromIntegral (byteTag (section HM.! "Y")) in
                        -- if god isn't dead then this should always be multiple of 256 
                        let base = ((y*16)-chunkYMin)*16*16 in 
                        if chunkVer >= 1451 then 
                            let blockThingies = 
                                    let daGoodStuff = 
                                            if chunkVer >= 2834 then 
                                                let blockComp = uncompoundTag . compoundTag <$> section HM.!? "block_states" in 
                                                let tagPalette = tagListItems . listTag <$> ((HM.!? "palette") =<< blockComp) in 
                                                let tagBlockStates = unhalfList . longArrayTag <$> ((HM.!? "data") =<< blockComp) in 
                                                (tagPalette, tagBlockStates) 
                                            else 
                                                (tagListItems . listTag <$> (section HM.!? "Palette"), unhalfList . longArrayTag <$> (section HM.!? "BlockStates"))
                                    in
                                    case daGoodStuff of 
                                        (Nothing, _) -> Nothing 
                                        (Just p, Nothing) -> 
                                            if not (null p) then 
                                                let blockTag = uncompoundTag (compoundTag (head p)) in 
                                                let (JavaText blockName) = stringTag (blockTag HM.! "Name") in 
                                                let block = BlockData (identifierOfText blockName) HM.empty in 
                                                Just (V.generate 4096 (\i -> (base + i, Just block)))
                                            else 
                                                Nothing 
                                        (Just p, Just s) ->
                                            let blockBits = fromIntegral (max ((length s * 64) `quot` 4096) 4) :: Word in 
                                            let perLong = 64 `quot` blockBits in 
                                            let generator i = 
                                                    let blockPid = 
                                                            if chunkVer >= 2529 then 
                                                                let longInd = fromIntegral i `quot` perLong in
                                                                let longSubInd = fromIntegral i `rem` perLong in 
                                                                let lvalue = fromIntegral @Int64 @Word64 (s !! fromIntegral longInd) in 
                                                                let shifted = lvalue `shiftR` fromIntegral (longSubInd * blockBits) in 
                                                                shifted .&. (-1 `shiftR` fromIntegral (64 - blockBits))


                                                            else
                                                                -- not today
                                                                undefined 
                                                    in 
                                                    let blockTag = uncompoundTag (compoundTag (p !! fromIntegral blockPid)) in 
                                                    let (JavaText blockName) = stringTag (blockTag HM.! "Name") in
                                                    let blockId = identifierOfText blockName in
                                                    let propertiesTag = uncompoundTag . compoundTag <$> blockTag HM.!? "Properties" in 
                                                    let block = 
                                                            case propertiesTag of 
                                                                Just t -> 
                                                                    BlockData blockId (HM.map (unjavaText . stringTag) t)
                                                                Nothing -> 
                                                                    BlockData blockId HM.empty 
                                                    in
                                                    block

                                            in  
                                            Just (V.generate 4096 (\i -> (base + i, Just $ generator i)))
                            in 
                            blockThingies
                        else undefined) <$> chunkSections
                in
                let blockData = foldl V.update (V.replicate blockNum Nothing) <$> chunkBlocks in 
                let biomes = 
                        if chunkVer < 2844 then 
                            let level = uncompoundTag (compoundTag (uncompoundTag chunkData HM.! "Level")) in 
                            let tagBiomes = 
                                    if chunkVer >= 1466 then 
                                        unhalfList (intArrayTag (level HM.! "Biomes"))
                                    else 
                                        let byteBiomes = unhalfList (byteArrayTag (level HM.! "Biomes")) in 
                                        map fromIntegral byteBiomes
                            in 
                            if not (null tagBiomes) then 
                                V.fromList [ 
                                    if chunkVer >= 2203 then 
                                        tagBiomes !! x `quot` 4 + (z `quot` 4) * 4 + fromIntegral ((y `quot` 4) * 4 * 4)
                                    else 
                                        tagBiomes !! x + z * 16 
                                | x <- [0..16], 
                                  z <- [0..16], 
                                  y <- [0..(chunkYMax - chunkYMin)]]
                            else 
                                V.replicate blockNum 1
                        else 
                            V.replicate blockNum 1
                in
                let root = uncompoundTag chunkData in 
                let entities = 
                        if chunkVer < 2844 then 
                            let level = uncompoundTag (compoundTag (root HM.! "Level")) in 
                            case level HM.!? "Entities" of 
                                Just e -> 
                                    tagListItems (listTag e)
                                Nothing -> 
                                    [] 
                        else 
                            case chunkEntityData of 
                                Just d -> 
                                    case uncompoundTag d HM.!? "Entities" of 
                                        Just v -> 
                                            tagListItems (listTag v) 
                                        Nothing -> []
                                Nothing -> []
                in
                let tileEntities = fromMaybe [] $
                        if chunkVer >= 2844 then 
                            tagListItems . listTag <$> root HM.!? "block_entities"
                        else 
                            let level = uncompoundTag (compoundTag (root HM.! "Level")) in 
                            tagListItems . listTag <$> level HM.!? "TileEntities"
                in 
                Blocks blockNum chunkYMin chunkYMax <$> blockData <*> pure biomes <*> pure (map compoundTag entities) <*> pure (map compoundTag tileEntities)
        else 
            Nothing
    where 
        fastConstruct :: Int -> Int -> Blocks 
        fastConstruct minY maxY = 
            let blockNum = 16 * 16 * abs (maxY - minY) in 
            let biome = V.replicate blockNum 1 in 
            Blocks blockNum minY maxY (V.replicate blockNum Nothing) biome [] []
