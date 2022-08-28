{-# LANGUAGE DeriveGeneric, DerivingVia, LambdaCase, DeriveAnyClass, GeneralizedNewtypeDeriving, ImportQualifiedPost, OverloadedStrings #-}
module Bulby.DatReader.NBTData where 

import Data.ByteString qualified as B
import Data.Binary
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Bits
import Data.Binary.Get
import Data.Binary.Put
import Data.Coerce 
import Control.Monad (replicateM, forM_)
import Data.Int
import GHC.Generics
import Data.HashMap.Strict qualified as HM
import Data.Hashable 
import Data.Bifoldable (bitraverse_)
import Data.Aeson
import Data.Scientific (fromRationalRepetend)
import Data.Either (either)
newtype IEEEDouble = IEEEDouble Double 
    deriving Show
    deriving newtype Hashable
newtype IEEEFloat  = IEEEFloat  Float
    deriving Show
    deriving newtype Hashable
newtype JavaText   = JavaText {unjavaText :: T.Text}
    deriving Show
    deriving newtype Hashable
newtype HalfList a = HalfList {unhalfList :: [a] }
    deriving (Show, Generic) 
instance Hashable a => Hashable (HalfList a)
instance Binary IEEEFloat where 
    put = putFloatbe . coerce 
    get = coerce <$> getFloatbe
instance Binary IEEEDouble where 
    put = putDoublebe . coerce 
    get = coerce <$> getDoublebe
getMany :: Binary a => Int -> Get [a] 
getMany = go [] 
    where 
        go xs 0 = pure $! reverse xs 
        go xs i = do 
            x <- get  
            x `seq` go (x:xs) (i - 1)
instance Binary a => Binary (HalfList a) where 
    get = do 
        n <- getInt32be 
        HalfList <$> getMany (fromIntegral n)
    put (HalfList xs) = do 
        putInt32be (fromIntegral (length xs))
        mapM_ put xs
instance Binary JavaText where 
    get = do 
        len <- getWord16be 
        bs <- getByteString (fromIntegral len)
        case T.decodeUtf8' bs of
            Left exn -> fail (show exn) 
            Right a -> pure (JavaText a)
    put (JavaText t) = 
        let encoded = T.encodeUtf8 t in 
            putWord16be (fromIntegral (B.length encoded)) <> 
            putByteString encoded 
data NBTTagList = NBTTagList { 
    kind :: Int8 
    , tagListItems :: [NBTTag] }
    deriving (Show, Generic) 
    deriving anyclass Hashable 

instance Binary NBTTagList where 
    get = do 
        kind <- getInt8 
        size <- getInt32be 
        NBTTagList kind <$> go (fromIntegral kind) [] (fromIntegral size) 
        where
            go :: Int -> [NBTTag] -> Int -> Get [NBTTag]
            go kind xs 0 = pure $! reverse xs 
            go kind xs i = do 
                daTag <- case kind of 
                    1 -> NBTByte (JavaText "") <$> get 
                    2 -> NBTShort (JavaText "") <$> get 
                    3 -> NBTInt (JavaText "") <$> get 
                    4 -> NBTLong (JavaText "") <$> get 
                    5 -> NBTFloat (JavaText "") <$> get 
                    6 -> NBTDouble (JavaText "") <$> get 
                    7 -> NBTByteArray (JavaText "") <$> get 
                    8 -> NBTString (JavaText "") <$> get 
                    9 -> NBTList (JavaText "") <$> get 
                    10 -> NBTCompound (JavaText "") <$> get 
                    11 -> NBTIntArray (JavaText "") <$> get 
                    12 -> NBTLongArray (JavaText "") <$> get 
                    _ -> fail "oopsies"
                daTag `seq` go kind (daTag:xs) (i - 1)
    put (NBTTagList kind xs) = do 
        put kind 
        putInt32be (fromIntegral (length xs)) 
        mapM_ (\case 
            NBTEnd -> undefined
            NBTByte _ b -> put b 
            NBTShort _ s -> put s 
            NBTInt _ s -> put s 
            NBTLong _ l -> put l 
            NBTFloat _ f -> put f 
            NBTDouble _ f -> put f 
            NBTByteArray _ a -> put a 
            NBTString _ s -> put s 
            NBTList _ l -> put l 
            NBTCompound _ c -> put c 
            NBTIntArray _ a -> put a 
            NBTLongArray _ a -> put a 
    
            ) xs
instance Binary NBTCompoundTag where 
    get = do 
        NBTCompoundTag <$> fetch HM.empty
        where 
        fetch xs = do 
            tag <- get 
            case tag of 
                NBTEnd -> pure xs
                _ ->
                    let (JavaText tagName) = tagname tag in 
                        fetch (HM.insert tagName tag xs)


    put (NBTCompoundTag objs) = do 
        forM_ (HM.toList objs) (\(k, v) -> put (v { tagname = JavaText k}))
        put NBTEnd
newtype NBTCompoundTag = NBTCompoundTag { uncompoundTag :: HM.HashMap T.Text NBTTag }
    deriving (Show, Generic) 
    deriving anyclass Hashable
data NBTTag
    = NBTEnd 
    | NBTByte  
        { tagname :: JavaText 
        , byteTag :: Int8 }
    | NBTShort 
        { tagname :: JavaText 
        , shortTag :: Int16}
    | NBTInt  
        { tagname :: JavaText 
        , intTag :: Int32 }
    | NBTLong 
        { tagname :: JavaText 
        , longTag :: Int64 }
    | NBTFloat 
        { tagname :: JavaText 
        , floatTag :: IEEEFloat }
    | NBTDouble 
        { tagname :: JavaText 
        , doubleTag :: IEEEDouble }
    | NBTByteArray 
        { tagname :: JavaText 
        , byteArrayTag :: HalfList Int8 } 
    | NBTString 
        { tagname :: JavaText 
        , stringTag :: JavaText }
    -- NBTTags in here don't have names
    | NBTList 
        { tagname :: JavaText
        , listTag :: NBTTagList }
    | NBTCompound 
        { tagname :: JavaText 
        , compoundTag :: NBTCompoundTag } 
    | NBTIntArray { 
        tagname :: JavaText, 
        intArrayTag :: HalfList Int32 } 
    | NBTLongArray { 
        tagname :: JavaText 
        ,longArrayTag :: HalfList Int64 }
    deriving (Generic, Show)
    deriving anyclass (Binary, Hashable)
-- no FromJSON, this is only for pretty printing
instance ToJSON NBTTag where 
    -- NBTEnd should never show up 
    toJSON NBTEnd = Null 
    toJSON (NBTByte _ byte) = Number (fromIntegral byte) 
    toJSON (NBTShort _ short) = toJSON short
    toJSON (NBTInt _ int) = toJSON int  
    toJSON (NBTLong _ long) = toJSON long
    toJSON (NBTFloat _ (IEEEFloat float)) = toJSON float
    toJSON (NBTDouble _ (IEEEDouble double)) = toJSON double
    toJSON (NBTByteArray _ (HalfList list)) = toJSON list
    toJSON (NBTString _ (JavaText txt)) = toJSON txt
    toJSON (NBTList _ (NBTTagList _ list)) = toJSON list 
    toJSON (NBTCompound _ (NBTCompoundTag hashmap)) = toJSON hashmap 
    toJSON (NBTIntArray _ (HalfList list)) = toJSON list 
    toJSON (NBTLongArray _ (HalfList list)) = toJSON list

