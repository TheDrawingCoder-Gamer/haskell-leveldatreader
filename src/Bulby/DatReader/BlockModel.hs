{-# LANGUAGE OverloadedStrings, TypeApplications, ScopedTypeVariables, DeriveGeneric, DerivingVia, DeriveAnyClass, LambdaCase #-}
module Bulby.DatReader.BlockModel where 

import Bulby.DatReader.Region (Identifier(Identifier), identifierOfText)
import Data.HashMap.Strict qualified as HM 
import Data.Text qualified as T
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Aeson.Types qualified as A
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Scientific qualified as S
import Control.Applicative ((<|>))
import Data.Coerce (coerce)
import GHC.Generics (Generic)
data BlockModelReference = BlockModelReference 
    { bmrModel :: Identifier
    , bmrX :: Int  
    , bmrY :: Int 
    , bmrUvlock :: Bool 
    , bmrWeight :: Int }
    deriving Show 
data MultiPartPart = MultiPartPart 
    { mppWhen :: Maybe (Either (HM.HashMap T.Text T.Text) [HM.HashMap T.Text T.Text])
    , mppApply :: Either BlockModelReference [BlockModelReference] }
data BlockState 
    = BSVariants 
    { variants :: HM.HashMap T.Text (Either BlockModelReference [BlockModelReference]) } 
    | BSMultipart 
    { parts :: [MultiPartPart] }

lookupValue :: (Eq k, Hashable k) => (A.Value -> A.Parser v) -> k -> HM.HashMap k A.Value -> A.Parser (Maybe v) 
lookupValue conv k hm = 
    let val = hm HM.!? k in 
    case val of 
        Nothing -> 
            pure Nothing 
        Just v -> 
            Just <$> conv v 
lookupText :: (Eq k, Hashable k) => k -> HM.HashMap k A.Value -> A.Parser (Maybe T.Text) 
lookupText = lookupValue (A.withText "String" pure) 
lookupInt :: (Eq k, Hashable k) => k -> HM.HashMap k A.Value -> A.Parser (Maybe Int) 
lookupInt = 
    lookupValue (A.withScientific "Int" (\x -> 
        if S.isInteger x then 
            case S.toBoundedInteger x of 
                Just v -> pure v 
                Nothing -> fail "Integer is outside of range"
        else 
            fail "Not an integer" 
    ))
lookupBool = lookupValue (A.withBool "Bool" pure)
instance A.FromJSON BlockModelReference where 
    parseJSON = A.withObject "BlockModelReference" (\obj -> do  
        let hashmap = A.toHashMapText obj  
        model <- lookupText "model" hashmap 
        case model of 
            Nothing -> fail "Model id is required" 
            Just m -> do
                x <- fromMaybe 0 <$> lookupInt "x" hashmap
                y <- fromMaybe 0 <$> lookupInt "y" hashmap
                uvlock <- fromMaybe False <$> lookupBool "uvlock" hashmap
                weight <- fromMaybe 1 <$> lookupInt "weight" hashmap
                pure (BlockModelReference (identifierOfText m) x y uvlock weight)
        )
instance A.FromJSON MultiPartPart where 
    parseJSON = A.withObject "MultiPartPart" (\vobj -> do 
        let obj = A.toHashMapText vobj 
        let when = obj HM.!? "when" 
        whenCase <- 
            case when of 
                Just v -> 
                    Just . Left <$> A.parseJSON @(HM.HashMap T.Text T.Text) v <|> A.withObject "When Case" (\x -> do
                    let orCase = A.toHashMapText x HM.!? "OR"
                    case orCase of 
                        Nothing -> fail "Expected states or OR case"
                        Just v -> Just . Right <$> A.parseJSON @[HM.HashMap T.Text T.Text] v 
                        ) v
                     
                Nothing -> pure Nothing
        let applyval = obj HM.!? "apply" 
        applyobj <- 
            case applyval of 
                Just v -> 
                    Left <$> A.parseJSON @BlockModelReference v <|> Right <$> A.parseJSON @[BlockModelReference] v
                Nothing -> 
                    fail "Apply Obj is required"
        pure (MultiPartPart whenCase applyobj)
        )

instance A.FromJSON BlockState where 
    parseJSON = A.withObject "BlockState" $ \v -> do
        multipart <- v A..:! "multipart" :: A.Parser (Maybe [MultiPartPart])
        variants  <- v A..:! "variants" :: A.Parser (Maybe (HM.HashMap T.Text (UntaggedEither BlockModelReference [BlockModelReference])))
        case (multipart, variants) of 
            (Just _, Just _) -> fail "Can't specify both multipart and variants" 
            (Just m, Nothing) -> 
                pure (BSMultipart m)
            (Nothing, Just v) -> 
                pure $ BSVariants (coerce @(HM.HashMap T.Text (UntaggedEither BlockModelReference [BlockModelReference])) @(HM.HashMap T.Text (Either BlockModelReference [BlockModelReference])) v)
            (Nothing, Nothing) -> 
                fail "Must specify variants or multipart"

newtype UntaggedEither a b = UntaggedEither (Either a b)

instance (A.FromJSON a, A.FromJSON b) => A.FromJSON (UntaggedEither a b) where 
    parseJSON val = 
        (UntaggedEither . Left <$> A.parseJSON @a val) 
        <|> (UntaggedEither . Right <$> A.parseJSON @b val)

data BlockModel = BlockModel 
    { bmParent :: Maybe Identifier 
    , bmAmbientOcclusion :: Bool 
    -- omitting display, unneeded 
    , bmTextures :: HM.HashMap T.Text T.Text 
    , bmElements :: [BlockPart]
    }
data Axis = 
    XAxis 
    | YAxis 
    | ZAxis 
instance A.FromJSON Axis where 
    parseJSON = A.withText "Axis" $ \case 
        "x" -> pure XAxis 
        "y" -> pure YAxis 
        "z" -> pure ZAxis 
        _ -> fail "Invalid axis"
data RotationInfo = RotationInfo 
    { riOrigin :: (Int, Int, Int) 
    , riAxis :: Axis 
    , riAngle :: Float 
    , riRescale :: Bool }

data BlockFace = 
    BFUp 
    | BFDown 
    | BFNorth 
    | BFEast 
    | BFSouth 
    | BFWest 
    deriving (Eq, Enum, Generic)
    deriving anyclass Hashable
newtype BlockFaces = BlockFaces [BlockFace]
instance A.FromJSON BlockFace where 
    parseJSON = A.withText "BlockFace" $ \case 
        "up" -> pure BFUp 
        "down" -> pure BFDown 
        "north" -> pure BFNorth 
        "east" -> pure BFEast 
        "south" -> pure BFSouth 
        "west" -> pure BFWest 
        "bottom" -> pure BFDown 
        _ -> fail "Invalid direction"
instance A.FromJSONKey BlockFace
data BlockFaceInfo = BlockFaceInfo 
    { bfiUv :: (Int, Int, Int, Int) 
    , bfiTexture :: T.Text 
    , bfiCullface :: Maybe BlockFace 
    , bfiRotation :: Int 
    , bfiTintIndex :: Maybe Int }

data BlockPart = BlockPart 
    { bpFrom :: (Int, Int, Int) 
    , bpTo   :: (Int, Int, Int)
    , bpRotation :: Maybe RotationInfo 
    , bpShade :: Bool 
    , bpFaces :: HM.HashMap BlockFace BlockFaceInfo }

