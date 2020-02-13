{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-} -- Required with ghc 8.4.3
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Format.HDF.SD(
    SDFile
  , HDFio
  , SomeSDS(..)
  , SDataSetId

  , withExistingSDHDF
  , withNewSDHDF

  , withExistingSDS
  , withNewSDS

  , sd_checkempty
  , sd_fileinfo
  , sd_getnamelen
  , sd_getfilename
  , sd_getinfo
  , sd_get_maxopenfiles
  , sd_get_numopenfiles
  , sd_getnumvars_byname
  , sd_idtoref
  , sd_iscoordvar
  , sd_isrecord
  , sd_nametoindices
  , sd_reftoindex
  , sd_reset_maxopenfiles
  , sd_getdimid
  , sd_diminfo
  , sd_setdimname
  , sd_findattr
  , sd_attrinfo
  , sd_getcal
  , sd_getdatastrs
  , sd_getdimstrs
  , sd_getfillvalue
  , sd_getrange
  , sd_setcal
  , sd_setdatastrs
  , sd_setdimstrs
  , sd_setfillvalue
  , sd_setfillmode
  , sd_setrange
  , sd_setcompress
  , sd_getcompinfo
  , sd_setnbitdataset
  , sd_setchunkcache
  , sd_setchunk
  , sd_getchunkinfo
  , sd_getanndatainfo
  , sd_getattdatainfo
  , sd_getoldattdatainfo
  , sd_getdatainfo
  , sd_getexternalinfo
  , sd_setblocksize
  , sd_setexternalfile
  , sd_isdimval_bwcomp
  , sd_setdimval_comp
  , sd_setdimscale
  , sd_getdimscale
  , sd_setattr
  , sd_readattr
  , sd_writechunk
  , sd_readchunk
  , sd_readdata
  , sd_writedata
    ) where

import           Control.Monad.Catch
import           Control.Monad.Except
import           Data.Int
import           Data.Kind
import           Data.Proxy
import qualified Data.Vector.Storable as VS
import           Foreign.Storable (Storable)
import           GHC.TypeLits (TypeError, ErrorMessage(..))
import           GHC.TypeNats (Nat, KnownNat)

import           Data.Format.HDF.LowLevel.Definitions
import           Data.Format.HDF.LowLevel.Definitions.Internal hiding (SDObjectId, getRawObjectId)
import qualified Data.Format.HDF.LowLevel.Definitions.Internal as Internal (SDObjectId)
import           Data.Format.HDF.LowLevel.HE
import qualified Data.Format.HDF.LowLevel.SD as LowLevel.SD

newtype SDFile (mode :: HDFOpenMode) s = SDFile LowLevel.SD.SDId

newtype SDataSetId (n :: Nat) (t :: HDataType a) s1 (mode :: HDFOpenMode) s2 where
 SDataSetId :: LowLevel.SD.SDataSetId n t -> SDataSetId n t s1 mode s2

data SomeSDS s1 (mode :: HDFOpenMode) s2 where
    SomeSDS :: forall (n :: Nat) (t :: HDataType a) s1 (mode :: HDFOpenMode) s2. KnownNat n =>
        HDataType a -> SDataSetId n t s1 mode s2 -> SomeSDS s1 mode s2

newtype SDimensionId (mode :: HDFOpenMode) s = SDimensionId LowLevel.SD.SDimensionId

class SDObjectId (id :: HDFOpenMode -> k -> Type) where
    type LLId id
    getRawObjectId :: id m s -> LLId id

instance SDObjectId SDFile where
    type LLId SDFile = LowLevel.SD.SDId
    getRawObjectId (SDFile file) = file

instance SDObjectId (SDataSetId n t s1) where
    type LLId (SDataSetId n t s1) = LowLevel.SD.SDataSetId n t
    getRawObjectId (SDataSetId sds) = sds

instance SDObjectId SDimensionId where
    type LLId SDimensionId = LowLevel.SD.SDimensionId
    getRawObjectId (SDimensionId dim) = dim

type HDFio a = ExceptT HDFError IO a

class SingI (t :: Type) (a :: t) where
  fromSing :: Proxy a -> t

instance SingI HDFOpenMode 'HDFRead   where
  fromSing _ = HDFRead
instance SingI HDFOpenMode 'HDFWrite  where
  fromSing _ = HDFWrite
instance SingI HDFOpenMode 'HDFCreate where
  fromSing _ = HDFCreate

withNewSDHDF :: forall b.
  String -> (forall session. SDFile 'HDFWrite session -> HDFio b) -> IO (Either HDFError b)
withNewSDHDF fileName action = runExceptT $ bracket
    (sd_start fileName HDFCreate) sd_end action

withExistingSDHDF :: forall (any :: HDFOpenMode) b. (SingI HDFOpenMode any, any `OneOf` ['HDFRead, 'HDFWrite]) =>
  String -> (forall session. SDFile any session -> HDFio b) -> IO (Either HDFError b)
withExistingSDHDF fileName action = runExceptT $ bracket
    (sd_start fileName openMode) sd_end action
  where
    openMode :: HDFOpenMode
    openMode = fromSing (Proxy :: Proxy any)


check :: (Int32, a) -> HDFio a
check (returnCode, val) = if returnCode == (-1)
    then (liftIO $ he_value 1) >>= throwError
    else return val

withExistingSDS :: forall (some :: HDFOpenMode) b session. SingI HDFOpenMode some =>
    SDFile some session -> String -> (forall sds. SomeSDS sds some session -> HDFio b) -> HDFio b
withExistingSDS sd sdsName action = do
    bracket
        (sd_select_by_name sd sdsName)
        (\(SomeSDS _ sds) -> sd_endaccess sds)
        action

type family CompatibleWith (newMode :: HDFOpenMode) (oldMode :: HDFOpenMode) :: Constraint where
    CompatibleWith 'HDFRead   _          = 'True  ~ 'True
    CompatibleWith 'HDFCreate 'HDFCreate = 'True  ~ 'True
    CompatibleWith 'HDFWrite  'HDFWrite  = 'True  ~ 'True
    CompatibleWith 'HDFWrite  'HDFCreate = 'True  ~ 'True
    CompatibleWith  l          r         =  TypeError ('ShowType l ':<>: 'Text " SDS access mode can not be used with SD file in " ':<>: 'ShowType r ':<>: 'Text " mode")


withNewSDS :: forall (t :: HDataType a) (n :: Nat) (some :: HDFOpenMode) b session. (SingI HDFOpenMode some, KnownNat n, 'HDFWrite `CompatibleWith` some) =>
    SDFile some session
 -> String
 -> HDataType a
 -> Index n
 -> (forall sds. SDataSetId n t sds 'HDFWrite session -> HDFio b)
 -> HDFio b
withNewSDS sd sdsName dataType dimSizes action = do
    bracket
        (sd_create sd sdsName dataType dimSizes)
        sd_endaccess
        action

sd_start :: String -> HDFOpenMode -> HDFio (SDFile any session)
sd_start fileName openMode = do
    fileId <- check =<< (liftIO $ LowLevel.SD.sd_start fileName openMode)
    return (SDFile fileId)

sd_end :: SDFile any session -> HDFio ()
sd_end (SDFile file) = check =<< (liftIO $ LowLevel.SD.sd_end file)

sd_create :: forall (t :: HDataType a) (n :: Nat) sds (some :: HDFOpenMode) session. (KnownNat n, 'HDFWrite `CompatibleWith` some) =>
    SDFile some session -> String -> HDataType a -> Index n -> HDFio (SDataSetId n t sds 'HDFWrite session)
sd_create (SDFile file) sdsName dataType dimSizes = do
    sds <- check =<< (liftIO $ LowLevel.SD.sd_create file sdsName dataType dimSizes)
    return $! SDataSetId sds

sd_select :: SDFile any session -> Int32 -> HDFio (SomeSDS sds any session)
sd_select (SDFile file) sdsIndex = do
    (LowLevel.SD.SomeSDS t sds) <- check =<< (liftIO $ LowLevel.SD.sd_select file sdsIndex)
    return $! SomeSDS t (SDataSetId sds)

sd_select_by_name :: SDFile any session -> String -> HDFio (SomeSDS sds any session)
sd_select_by_name sd sdsName = sd_nametoindex sd sdsName >>= sd_select sd

sd_endaccess :: SDataSetId n t sds any session -> HDFio ()
sd_endaccess (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_endaccess sds)

sd_checkempty :: SDataSetId n t sds any session -> HDFio Bool
sd_checkempty (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_checkempty sds)

sd_fileinfo :: SDFile any session -> HDFio (Int32, Int32)
sd_fileinfo (SDFile file) = check =<< (liftIO $ LowLevel.SD.sd_fileinfo file)

sd_getnamelen :: (Internal.SDObjectId (LLId id), SDObjectId id) =>
    (id any session) -> HDFio Int32
sd_getnamelen obj = check =<< (liftIO $ LowLevel.SD.sd_getnamelen $ getRawObjectId obj)

sd_getfilename :: SDFile any session -> HDFio String
sd_getfilename (SDFile file) = check =<< (liftIO $ LowLevel.SD.sd_getfilename file)

sd_getinfo :: SDataSetId n t sds any session -> HDFio LowLevel.SD.SDataSetInfoRaw
sd_getinfo (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getinfo sds)

sd_get_maxopenfiles :: HDFio (Int32, Int32)
sd_get_maxopenfiles = check =<< liftIO LowLevel.SD.sd_get_maxopenfiles

sd_get_numopenfiles :: HDFio Int32
sd_get_numopenfiles = check =<< liftIO LowLevel.SD.sd_get_numopenfiles

sd_getnumvars_byname :: SDFile any session -> String -> HDFio Int32
sd_getnumvars_byname (SDFile file) sdsName = check =<< (liftIO $ LowLevel.SD.sd_getnumvars_byname file sdsName)

sd_idtoref :: SDataSetId n t sds any session -> HDFio LowLevel.SD.SDataSetRef
sd_idtoref (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_idtoref sds)

sd_iscoordvar :: SDataSetId n t sds any session -> HDFio Bool
sd_iscoordvar (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_iscoordvar sds)

sd_isrecord :: SDataSetId n t sds any session -> HDFio Bool
sd_isrecord (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_isrecord sds)

sd_nametoindex :: SDFile any session -> String -> HDFio Int32
sd_nametoindex (SDFile file) sdsName = check =<< (liftIO $ LowLevel.SD.sd_nametoindex file sdsName)

sd_nametoindices :: SDFile any session -> String -> HDFio [LowLevel.SD.HDFVarList]
sd_nametoindices (SDFile file) sdsName = check =<< (liftIO $ LowLevel.SD.sd_nametoindices file sdsName)

sd_reftoindex :: SDFile any session -> LowLevel.SD.SDataSetRef -> HDFio Int32
sd_reftoindex (SDFile file) sdsRef = check =<< (liftIO $ LowLevel.SD.sd_reftoindex file sdsRef)

sd_reset_maxopenfiles :: Int32 -> HDFio Int32
sd_reset_maxopenfiles newLimit = check =<< (liftIO $ LowLevel.SD.sd_reset_maxopenfiles newLimit)

sd_getdimid :: SDataSetId n t sds any session -> Int32 -> HDFio (SDimensionId any session)
sd_getdimid (SDataSetId sds) dimIndex = do
    dim <- check =<< (liftIO $ LowLevel.SD.sd_getdimid sds dimIndex)
    return $! SDimensionId dim

sd_diminfo :: SDimensionId any session -> HDFio LowLevel.SD.SDimensionInfoRaw
sd_diminfo (SDimensionId dim) = check =<< (liftIO $ LowLevel.SD.sd_diminfo dim)

sd_setdimname :: forall session.
    SDimensionId 'HDFWrite session -> String -> HDFio ()
sd_setdimname (SDimensionId dim) dimName = check =<< (liftIO $ LowLevel.SD.sd_setdimname dim dimName)

sd_findattr :: (Internal.SDObjectId (LLId id), SDObjectId id) =>
    id any session -> String -> HDFio Int32
sd_findattr obj attrName = check =<< (liftIO $ LowLevel.SD.sd_findattr (getRawObjectId obj) attrName)

sd_attrinfo :: (Internal.SDObjectId (LLId id), SDObjectId id) =>
    id any session -> Int32 -> HDFio LowLevel.SD.SAttributeInfoRaw
sd_attrinfo obj attrId = check =<< (liftIO $ LowLevel.SD.sd_attrinfo (getRawObjectId obj) attrId)

sd_getcal :: SDataSetId n t sds any session -> HDFio LowLevel.SD.SCalibrationParametersRaw
sd_getcal (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getcal sds)

sd_getdatastrs :: SDataSetId n t sds any session -> HDFio LowLevel.SD.SDsetDescStringsRaw
sd_getdatastrs (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getdatastrs sds)

sd_getdimstrs :: SDimensionId any session -> HDFio LowLevel.SD.SDimDescStringsRaw
sd_getdimstrs (SDimensionId dim) = check =<< (liftIO $ LowLevel.SD.sd_getdimstrs dim)

sd_getfillvalue :: forall (t :: HDataType a) (n :: Nat) sds any session. Storable a =>
    SDataSetId n t sds any session -> HDFio a
sd_getfillvalue (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getfillvalue sds)

sd_getrange :: forall (t :: HDataType a) (n :: Nat) sds any session. Storable a =>
    SDataSetId n t sds any session -> HDFio (a, a)
sd_getrange (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getrange sds)

sd_setcal :: forall (t :: HDataType a) (n :: Nat) sds session.
    SDataSetId n t sds 'HDFWrite session -> LowLevel.SD.SCalibrationParametersRaw -> HDFio ()
sd_setcal (SDataSetId sds) calibrationParameters = check =<< (liftIO $ LowLevel.SD.sd_setcal sds calibrationParameters)

sd_setdatastrs :: forall (t :: HDataType a) (n :: Nat) sds session.
    SDataSetId n t sds 'HDFWrite session -> LowLevel.SD.SDsetDescStringsRaw -> HDFio ()
sd_setdatastrs (SDataSetId sds) sdsDescription = check =<< (liftIO $ LowLevel.SD.sd_setdatastrs sds sdsDescription)

sd_setdimstrs:: forall session.
    SDimensionId 'HDFWrite session -> LowLevel.SD.SDimDescStringsRaw -> HDFio ()
sd_setdimstrs (SDimensionId dim) dimDescription = check =<< (liftIO $ LowLevel.SD.sd_setdimstrs dim dimDescription)

sd_setfillvalue :: forall (t :: HDataType a) (n :: Nat) sds session. Storable a =>
    SDataSetId n t sds 'HDFWrite session -> a -> HDFio ()
sd_setfillvalue (SDataSetId sds) fillValue = check =<< (liftIO $ LowLevel.SD.sd_setfillvalue sds fillValue)

sd_setfillmode :: forall session.
    SDFile 'HDFWrite session -> HDFFillMode -> HDFio ()
sd_setfillmode (SDFile file) fillMode = check =<< (liftIO $ LowLevel.SD.sd_setfillmode file fillMode)

sd_setrange :: forall (t :: HDataType a) (n :: Nat) sds session. Storable a =>
    SDataSetId n t sds 'HDFWrite session -> a -> a -> HDFio ()
sd_setrange (SDataSetId sds) minValue maxValue = check =<< (liftIO $ LowLevel.SD.sd_setrange sds minValue maxValue)

sd_setcompress :: forall (t :: HDataType a) (n :: Nat) sds session.
    SDataSetId n t sds 'HDFWrite session -> LowLevel.SD.HDFCompParams -> HDFio ()
sd_setcompress (SDataSetId sds) compParams = check =<< (liftIO $ LowLevel.SD.sd_setcompress sds compParams)

sd_getcompinfo :: SDataSetId n t sds any session -> HDFio LowLevel.SD.HDFCompParams
sd_getcompinfo (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getcompinfo sds)

sd_setnbitdataset :: forall (t :: HDataType a) (n :: Nat) sds session.
    SDataSetId n t sds 'HDFWrite session -> LowLevel.SD.SDNBitCompParams -> HDFio ()
sd_setnbitdataset (SDataSetId sds) compParams = check =<< (liftIO $ LowLevel.SD.sd_setnbitdataset sds compParams)

sd_setchunkcache :: SDataSetId n t sds any session -> Int32 -> HDFio ()
sd_setchunkcache (SDataSetId sds) cacheSize = check =<< (liftIO $ LowLevel.SD.sd_setchunkcache sds cacheSize)

sd_setchunk :: forall (t :: HDataType a) (n :: Nat) sds session.
    SDataSetId n t sds 'HDFWrite session -> LowLevel.SD.HDFChunkParams -> HDFio ()
sd_setchunk (SDataSetId sds) chunkParams = check =<< (liftIO $ LowLevel.SD.sd_setchunk sds chunkParams)

sd_getchunkinfo :: SDataSetId n t sds any session -> HDFio LowLevel.SD.HDFChunkParams
sd_getchunkinfo (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getchunkinfo sds)

type family WrapIfSDS (a :: Type) where
    WrapIfSDS (LowLevel.SD.SDataSetId _ _) = LowLevel.SD.SomeSDS
    WrapIfSDS t = t

sd_getanndatainfo :: (Internal.SDObjectId (LLId id), SDObjectId id, WrapIfSDS (LLId id) `OneOf` '[LowLevel.SD.SomeSDS, LowLevel.SD.SDId]) =>
    id any session -> HDFAnnotationType -> HDFio [LowLevel.SD.RawDataInfo]
sd_getanndatainfo obj annType = check =<< (liftIO $ LowLevel.SD.sd_getanndatainfo (getRawObjectId obj) annType)

sd_getattdatainfo :: (Internal.SDObjectId (LLId id), SDObjectId id) =>
    id any session -> Int32 -> HDFio LowLevel.SD.RawDataInfo
sd_getattdatainfo obj attrId = check =<< (liftIO $ LowLevel.SD.sd_getattdatainfo (getRawObjectId obj) attrId)

sd_getoldattdatainfo :: SDataSetId n t sds any session -> Maybe (SDimensionId any session) -> String -> HDFio LowLevel.SD.RawDataInfo
sd_getoldattdatainfo (SDataSetId sds) dim attrName = check =<< (liftIO $ LowLevel.SD.sd_getoldattdatainfo sds rawDim attrName)
  where
    rawDim = fmap (\(SDimensionId d) -> d) dim

sd_getdatainfo :: SDataSetId n t sds any session -> [Int32] -> Int32 -> HDFio [LowLevel.SD.RawDataInfo]
sd_getdatainfo (SDataSetId sds) chunkCoords startBlock = check =<< (liftIO $ LowLevel.SD.sd_getdatainfo sds chunkCoords startBlock)

sd_getexternalinfo :: SDataSetId n t sds any session -> HDFio (String, LowLevel.SD.RawDataInfo)
sd_getexternalinfo (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getexternalinfo sds)

sd_setblocksize :: forall (t :: HDataType a) (n :: Nat) sds session.
    SDataSetId n t sds 'HDFWrite session -> Int32 -> HDFio ()
sd_setblocksize (SDataSetId sds) blockSize = check =<< (liftIO $ LowLevel.SD.sd_setblocksize sds blockSize)

sd_setexternalfile :: forall (t :: HDataType a) (n :: Nat) sds session.
    SDataSetId n t sds 'HDFWrite session -> String -> Int32 -> HDFio ()
sd_setexternalfile (SDataSetId sds) fileName offset = check =<< (liftIO $ LowLevel.SD.sd_setexternalfile sds fileName offset)

sd_isdimval_bwcomp :: SDimensionId any session -> HDFio Bool
sd_isdimval_bwcomp (SDimensionId dim) = check =<< (liftIO $ LowLevel.SD.sd_isdimval_bwcomp dim)

sd_setdimval_comp :: forall session.
    SDimensionId 'HDFWrite session -> Bool -> HDFio ()
sd_setdimval_comp (SDimensionId dim) bwCompatible = check =<< (liftIO $ LowLevel.SD.sd_setdimval_comp dim bwCompatible)

sd_setdimscale :: forall a session. (Storable a) =>
    SDimensionId 'HDFWrite session -> HDataType a -> VS.Vector a -> HDFio ()
sd_setdimscale (SDimensionId dim) dataType dimScale = check =<< (liftIO $ LowLevel.SD.sd_setdimscale dim dataType dimScale)

sd_getdimscale :: SDataSetId n t sds any session -> SDimensionId any session -> HDFio HDFVector
sd_getdimscale (SDataSetId sds) (SDimensionId dim) = check =<< (liftIO $ LowLevel.SD.sd_getdimscale sds dim)

sd_setattr :: forall id (t :: Type) (a :: Type) session.
    (Internal.SDObjectId (LLId id), SDObjectId id, Storable a, CanBeAttribute t, a `IsElementOf` t) =>
    id 'HDFWrite session -> String -> HDataType a -> t -> HDFio ()
sd_setattr obj attrName dataType attrValue = check =<< (liftIO $ LowLevel.SD.sd_setattr (getRawObjectId obj) attrName dataType attrValue)

sd_readattr :: (Internal.SDObjectId (LLId id), SDObjectId id) =>
    id any session -> Int32 -> HDFio HDFVector
sd_readattr obj attrId = check =<< (liftIO $ LowLevel.SD.sd_readattr (getRawObjectId obj) attrId)

sd_writechunk :: forall (t :: HDataType a) (n :: Nat) sds session. Storable a =>
    SDataSetId n t sds 'HDFWrite session -> [Int32] -> VS.Vector a -> HDFio ()
sd_writechunk (SDataSetId sds) chunkCoords dataChunk = check =<< (liftIO $ LowLevel.SD.sd_writechunk sds chunkCoords dataChunk)

sd_readchunk :: forall (t :: HDataType a) (n :: Nat) sds any session. Storable a =>
    SDataSetId n t sds any session -> [Int32] -> HDFio (VS.Vector a)
sd_readchunk (SDataSetId sds) chunkCoords = check =<< (liftIO $ LowLevel.SD.sd_readchunk sds chunkCoords)

sd_writedata :: forall (t :: HDataType a) (n :: Nat) sds session. (Storable a, KnownNat n) =>
    SDataSetId n t sds 'HDFWrite session -> Index n -> Index n -> Index n -> VS.Vector a -> HDFio ()
sd_writedata (SDataSetId sds) start stride edges sdsData = check =<< (liftIO $ LowLevel.SD.sd_writedata sds start stride edges sdsData)

sd_readdata :: forall (t :: HDataType a) (n :: Nat) sds any session. (Storable a, KnownNat n) =>
    SDataSetId n t sds any session -> Index n -> Index n -> Index n -> HDFio (VS.Vector a)
sd_readdata (SDataSetId sds) start stride edges = check =<< (liftIO $ LowLevel.SD.sd_readdata sds start stride edges)
