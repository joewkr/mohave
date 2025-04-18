{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Format.HDF.SD(
    SDFile
  , HDFio
  , SomeSDS(..)
  , SDataSetId
  , SDimensionId

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

  , module InternalDefs
    ) where

import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.IO.Class (MonadIO, liftIO)
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
import           Internal.Definitions
import qualified Internal.Definitions as InternalDefs (
                      StaticVector(..)
                    , TypedValue(..)
                    , pattern Var0D
                    , pattern Var1D
                    , pattern Var2D
                    , pattern Var3D
                    , pattern Var4D
                    , pattern Var5D
                    , pattern Var6D)

newtype SDFile (mode :: HDFOpenMode) s = SDFile LowLevel.SD.SDId

newtype SDataSetId (n :: Nat) (t :: HDataType a) (mode :: HDFOpenMode) s where
 SDataSetId :: LowLevel.SD.SDataSetId n t -> SDataSetId n t mode s

data SomeSDS (mode :: HDFOpenMode) s where
    SomeSDS :: forall a (n :: Nat) (t :: HDataType a) (mode :: HDFOpenMode) s. KnownNat n =>
        HDataTypeS t -> SDataSetId n t mode s -> SomeSDS mode s

newtype SDimensionId (mode :: HDFOpenMode) s = SDimensionId LowLevel.SD.SDimensionId

class SDObjectId (id :: HDFOpenMode -> k -> Type) where
    type LLId id
    getRawObjectId :: id m s -> LLId id

instance SDObjectId SDFile where
    type LLId SDFile = LowLevel.SD.SDId
    getRawObjectId (SDFile file) = file

instance SDObjectId (SDataSetId n t) where
    type LLId (SDataSetId n t) = LowLevel.SD.SDataSetId n t
    getRawObjectId (SDataSetId sds) = sds

instance SDObjectId SDimensionId where
    type LLId SDimensionId = LowLevel.SD.SDimensionId
    getRawObjectId (SDimensionId dim) = dim

newtype HDFio s a = HDFio {unHDFio :: ExceptT HDFError IO a}

instance Functor (HDFio s) where
  fmap f (HDFio io) = HDFio (f `fmap` io)

instance Applicative (HDFio s) where
  pure io = HDFio (pure io)
  (HDFio f) <*> (HDFio a) = HDFio (f <*> a)

instance Monad (HDFio s) where
  (HDFio a) >>= b = HDFio $ a >>= (unHDFio . b)

instance MonadIO (HDFio s) where
    liftIO a = HDFio (liftIO a)

instance MonadError HDFError (HDFio s) where
    throwError a = HDFio (throwError a)
    catchError (HDFio m) h = HDFio (m `catchError` (unHDFio . h))


class SingI (t :: Type) (a :: t) where
  fromSing :: Proxy a -> t

instance SingI HDFOpenMode 'HDFRead   where
  fromSing _ = HDFRead
instance SingI HDFOpenMode 'HDFWrite  where
  fromSing _ = HDFWrite
instance SingI HDFOpenMode 'HDFCreate where
  fromSing _ = HDFCreate

withNewSDHDF :: forall b.
  String -> (forall s. SDFile 'HDFWrite s -> HDFio s b) -> IO (Either HDFError b)
withNewSDHDF fileName action = runExceptT $ bracket
    (unHDFio $ sd_start fileName HDFCreate) (unHDFio . sd_end) (unHDFio . action)

withExistingSDHDF :: forall (any :: HDFOpenMode) b. (SingI HDFOpenMode any, any `OneOf` ['HDFRead, 'HDFWrite]) =>
  String -> (forall s. SDFile any s -> HDFio s b) -> IO (Either HDFError b)
withExistingSDHDF fileName action = runExceptT $ bracket
    (unHDFio $ sd_start fileName openMode) (unHDFio . sd_end) (unHDFio . action)
  where
    openMode :: HDFOpenMode
    openMode = fromSing (Proxy :: Proxy any)


check :: (Int32, a) -> HDFio s a
check (returnCode, val) = if returnCode == (-1)
    then HDFio $ (liftIO $ he_value 1) >>= throwError
    else return val

withExistingSDS :: forall (some :: HDFOpenMode) b s. SingI HDFOpenMode some =>
    SDFile some s
  -> String
  -> (forall r. SomeSDS some (r,s) -> HDFio (r,s) b)
  -> HDFio s b
withExistingSDS sd sdsName action = HDFio $ do
    bracket
        (unHDFio $ sd_select_by_name sd sdsName)
        (\(SomeSDS _ sds) -> unHDFio $ sd_endaccess sds)
        (unHDFio . action)

type family CompatibleWith (newMode :: HDFOpenMode) (oldMode :: HDFOpenMode) :: Constraint where
    CompatibleWith 'HDFRead   _          = ()
    CompatibleWith 'HDFCreate 'HDFCreate = ()
    CompatibleWith 'HDFWrite  'HDFWrite  = ()
    CompatibleWith 'HDFWrite  'HDFCreate = ()
    CompatibleWith  l          r         =  TypeError ('ShowType l ':<>: 'Text " SDS access mode can not be used with SD file in " ':<>: 'ShowType r ':<>: 'Text " mode")


withNewSDS :: forall a (t :: HDataType a) (n :: Nat) (some :: HDFOpenMode) b s. (SingI HDFOpenMode some, KnownNat n, 'HDFWrite `CompatibleWith` some) =>
    SDFile some s
 -> String
 -> HDataType a
 -> Index n
 -> (forall r. SDataSetId n t 'HDFWrite (r,s) -> HDFio (r,s) b)
 -> HDFio s b
withNewSDS sd sdsName dataType dimSizes action = HDFio $ do
    bracket
        (unHDFio $ sd_create sd sdsName dataType dimSizes)
        (unHDFio . sd_endaccess)
        (unHDFio . action)

sd_start :: String -> HDFOpenMode -> HDFio s (SDFile any s)
sd_start fileName openMode = do
    fileId <- check =<< (liftIO $ LowLevel.SD.sd_start fileName openMode)
    return (SDFile fileId)

sd_end :: SDFile any s -> HDFio s ()
sd_end (SDFile file) = check =<< (liftIO $ LowLevel.SD.sd_end file)

sd_create :: forall a (t :: HDataType a) (n :: Nat) (some :: HDFOpenMode) s r. (KnownNat n, 'HDFWrite `CompatibleWith` some) =>
     SDFile some s
  -> String
  -> HDataType a
  -> Index n
  -> HDFio s (SDataSetId n t 'HDFWrite r)
sd_create (SDFile file) sdsName dataType dimSizes = do
    sds <- check =<< (liftIO $ LowLevel.SD.sd_create file sdsName dataType dimSizes)
    return $! SDataSetId sds

sd_select :: SDFile any s -> Int32 -> HDFio s (SomeSDS any r)
sd_select (SDFile file) sdsIndex = do
    (LowLevel.SD.SomeSDS t sds) <- check =<< (liftIO $ LowLevel.SD.sd_select file sdsIndex)
    return $! SomeSDS t (SDataSetId sds)

sd_select_by_name :: SDFile any s -> String -> HDFio s (SomeSDS any r)
sd_select_by_name sd sdsName = sd_nametoindex sd sdsName >>= sd_select sd

sd_endaccess :: SDataSetId n t any (r,s) -> HDFio (r,s) ()
sd_endaccess (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_endaccess sds)

sd_checkempty :: SDataSetId n t any (r,s) -> HDFio (r,s) Bool
sd_checkempty (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_checkempty sds)

sd_fileinfo :: SDFile any s -> HDFio s (Int32, Int32)
sd_fileinfo (SDFile file) = check =<< (liftIO $ LowLevel.SD.sd_fileinfo file)

sd_getnamelen :: (Internal.SDObjectId (LLId id), SDObjectId id) =>
    (id any s) -> HDFio s Int32
sd_getnamelen obj = check =<< (liftIO $ LowLevel.SD.sd_getnamelen $ getRawObjectId obj)

sd_getfilename :: SDFile any s -> HDFio s String
sd_getfilename (SDFile file) = check =<< (liftIO $ LowLevel.SD.sd_getfilename file)

sd_getinfo :: SDataSetId n t any (r,s) -> HDFio (r,s) LowLevel.SD.SDataSetInfoRaw
sd_getinfo (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getinfo sds)

sd_get_maxopenfiles :: HDFio s (Int32, Int32)
sd_get_maxopenfiles = check =<< liftIO LowLevel.SD.sd_get_maxopenfiles

sd_get_numopenfiles :: HDFio s Int32
sd_get_numopenfiles = check =<< liftIO LowLevel.SD.sd_get_numopenfiles

sd_getnumvars_byname :: SDFile any s -> String -> HDFio s Int32
sd_getnumvars_byname (SDFile file) sdsName = check =<< (liftIO $ LowLevel.SD.sd_getnumvars_byname file sdsName)

sd_idtoref :: SDataSetId n t any (r,s) -> HDFio (r,s) LowLevel.SD.SDataSetRef
sd_idtoref (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_idtoref sds)

sd_iscoordvar :: SDataSetId n t any (r,s) -> HDFio (r,s) Bool
sd_iscoordvar (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_iscoordvar sds)

sd_isrecord :: SDataSetId n t any (r,s) -> HDFio (r,s) Bool
sd_isrecord (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_isrecord sds)

sd_nametoindex :: SDFile any s -> String -> HDFio s Int32
sd_nametoindex (SDFile file) sdsName = check =<< (liftIO $ LowLevel.SD.sd_nametoindex file sdsName)

sd_nametoindices :: SDFile any s -> String -> HDFio s [LowLevel.SD.HDFVarList]
sd_nametoindices (SDFile file) sdsName = check =<< (liftIO $ LowLevel.SD.sd_nametoindices file sdsName)

sd_reftoindex :: SDFile any s -> LowLevel.SD.SDataSetRef -> HDFio s Int32
sd_reftoindex (SDFile file) sdsRef = check =<< (liftIO $ LowLevel.SD.sd_reftoindex file sdsRef)

sd_reset_maxopenfiles :: Int32 -> HDFio s Int32
sd_reset_maxopenfiles newLimit = check =<< (liftIO $ LowLevel.SD.sd_reset_maxopenfiles newLimit)

sd_getdimid :: SDataSetId n t any (r,s) -> Int32 -> HDFio (r,s) (SDimensionId any s)
sd_getdimid (SDataSetId sds) dimIndex = do
    dim <- check =<< (liftIO $ LowLevel.SD.sd_getdimid sds dimIndex)
    return $! SDimensionId dim

sd_diminfo :: SDimensionId any s -> HDFio s LowLevel.SD.SDimensionInfoRaw
sd_diminfo (SDimensionId dim) = check =<< (liftIO $ LowLevel.SD.sd_diminfo dim)

sd_setdimname :: forall s.
    SDimensionId 'HDFWrite s -> String -> HDFio s ()
sd_setdimname (SDimensionId dim) dimName = check =<< (liftIO $ LowLevel.SD.sd_setdimname dim dimName)

sd_findattr :: (Internal.SDObjectId (LLId id), SDObjectId id) =>
    id any s -> String -> HDFio s Int32
sd_findattr obj attrName = check =<< (liftIO $ LowLevel.SD.sd_findattr (getRawObjectId obj) attrName)

sd_attrinfo :: (Internal.SDObjectId (LLId id), SDObjectId id) =>
    id any s -> Int32 -> HDFio s LowLevel.SD.SAttributeInfoRaw
sd_attrinfo obj attrId = check =<< (liftIO $ LowLevel.SD.sd_attrinfo (getRawObjectId obj) attrId)

sd_getcal :: SDataSetId n t any (r,s) -> HDFio (r,s) LowLevel.SD.SCalibrationParametersRaw
sd_getcal (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getcal sds)

sd_getdatastrs :: SDataSetId n t any (r,s) -> HDFio (r,s) LowLevel.SD.SDsetDescStringsRaw
sd_getdatastrs (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getdatastrs sds)

sd_getdimstrs :: SDimensionId any s -> HDFio s LowLevel.SD.SDimDescStringsRaw
sd_getdimstrs (SDimensionId dim) = check =<< (liftIO $ LowLevel.SD.sd_getdimstrs dim)

sd_getfillvalue :: forall a (t :: HDataType a) (n :: Nat) any r s. Storable a =>
    SDataSetId n t any (r,s) -> HDFio (r,s) a
sd_getfillvalue (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getfillvalue sds)

sd_getrange :: forall a (t :: HDataType a) (n :: Nat) any r s. Storable a =>
    SDataSetId n t any (r,s) -> HDFio (r,s) (a, a)
sd_getrange (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getrange sds)

sd_setcal :: forall a (t :: HDataType a) (n :: Nat) r s.
    SDataSetId n t 'HDFWrite (r,s) -> LowLevel.SD.SCalibrationParametersRaw -> HDFio (r,s) ()
sd_setcal (SDataSetId sds) calibrationParameters = check =<< (liftIO $ LowLevel.SD.sd_setcal sds calibrationParameters)

sd_setdatastrs :: forall a (t :: HDataType a) (n :: Nat) r s.
    SDataSetId n t 'HDFWrite (r,s) -> LowLevel.SD.SDsetDescStringsRaw -> HDFio (r,s) ()
sd_setdatastrs (SDataSetId sds) sdsDescription = check =<< (liftIO $ LowLevel.SD.sd_setdatastrs sds sdsDescription)

sd_setdimstrs:: forall s.
    SDimensionId 'HDFWrite s -> LowLevel.SD.SDimDescStringsRaw -> HDFio s ()
sd_setdimstrs (SDimensionId dim) dimDescription = check =<< (liftIO $ LowLevel.SD.sd_setdimstrs dim dimDescription)

sd_setfillvalue :: forall a (t :: HDataType a) (n :: Nat) r s. Storable a =>
    SDataSetId n t 'HDFWrite (r,s) -> a -> HDFio (r,s) ()
sd_setfillvalue (SDataSetId sds) fillValue = check =<< (liftIO $ LowLevel.SD.sd_setfillvalue sds fillValue)

sd_setfillmode :: forall s.
    SDFile 'HDFWrite s -> HDFFillMode -> HDFio s HDFFillMode
sd_setfillmode (SDFile file) fillMode = check =<< (liftIO $ LowLevel.SD.sd_setfillmode file fillMode)

sd_setrange :: forall a (t :: HDataType a) (n :: Nat) r s. Storable a =>
    SDataSetId n t 'HDFWrite (r,s) -> a -> a -> HDFio (r,s) ()
sd_setrange (SDataSetId sds) minValue maxValue = check =<< (liftIO $ LowLevel.SD.sd_setrange sds minValue maxValue)

sd_setcompress :: forall a (t :: HDataType a) (n :: Nat) r s.
    SDataSetId n t 'HDFWrite (r,s) -> LowLevel.SD.HDFCompParams -> HDFio (r,s) ()
sd_setcompress (SDataSetId sds) compParams = check =<< (liftIO $ LowLevel.SD.sd_setcompress sds compParams)

sd_getcompinfo :: SDataSetId n t any (r,s) -> HDFio (r,s) LowLevel.SD.HDFCompParams
sd_getcompinfo (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getcompinfo sds)

sd_setnbitdataset :: forall a (t :: HDataType a) (n :: Nat) r s.
    SDataSetId n t 'HDFWrite (r,s) -> LowLevel.SD.SDNBitCompParams -> HDFio (r,s) ()
sd_setnbitdataset (SDataSetId sds) compParams = check =<< (liftIO $ LowLevel.SD.sd_setnbitdataset sds compParams)

sd_setchunkcache :: SDataSetId n t any (r,s) -> Int32 -> HDFio (r,s) ()
sd_setchunkcache (SDataSetId sds) cacheSize = check =<< (liftIO $ LowLevel.SD.sd_setchunkcache sds cacheSize)

sd_setchunk :: forall a (t :: HDataType a) (n :: Nat) r s.
    SDataSetId n t 'HDFWrite (r,s) -> LowLevel.SD.HDFChunkParams -> HDFio (r,s) ()
sd_setchunk (SDataSetId sds) chunkParams = check =<< (liftIO $ LowLevel.SD.sd_setchunk sds chunkParams)

sd_getchunkinfo :: SDataSetId n t any (r,s) -> HDFio (r,s) LowLevel.SD.HDFChunkParams
sd_getchunkinfo (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getchunkinfo sds)

type family WrapIfSDS (a :: Type) where
    WrapIfSDS (LowLevel.SD.SDataSetId _ _) = LowLevel.SD.SomeSDS
    WrapIfSDS t = t

sd_getanndatainfo :: (Internal.SDObjectId (LLId id), SDObjectId id, WrapIfSDS (LLId id) `OneOf` '[LowLevel.SD.SomeSDS, LowLevel.SD.SDId]) =>
    id any s -> HDFAnnotationType -> HDFio s [LowLevel.SD.RawDataInfo]
sd_getanndatainfo obj annType = check =<< (liftIO $ LowLevel.SD.sd_getanndatainfo (getRawObjectId obj) annType)

sd_getattdatainfo :: (Internal.SDObjectId (LLId id), SDObjectId id) =>
    id any s -> Int32 -> HDFio s LowLevel.SD.RawDataInfo
sd_getattdatainfo obj attrId = check =<< (liftIO $ LowLevel.SD.sd_getattdatainfo (getRawObjectId obj) attrId)

sd_getoldattdatainfo :: SDataSetId n t any (r,s) -> Maybe (SDimensionId any s) -> String -> HDFio (r,s) LowLevel.SD.RawDataInfo
sd_getoldattdatainfo (SDataSetId sds) dim attrName = check =<< (liftIO $ LowLevel.SD.sd_getoldattdatainfo sds rawDim attrName)
  where
    rawDim = fmap (\(SDimensionId d) -> d) dim

sd_getdatainfo :: SDataSetId n t any (r,s) -> [Int32] -> Int32 -> HDFio (r,s) [LowLevel.SD.RawDataInfo]
sd_getdatainfo (SDataSetId sds) chunkCoords startBlock = check =<< (liftIO $ LowLevel.SD.sd_getdatainfo sds chunkCoords startBlock)

sd_getexternalinfo :: SDataSetId n t any (r,s) -> HDFio (r,s) (String, LowLevel.SD.RawDataInfo)
sd_getexternalinfo (SDataSetId sds) = check =<< (liftIO $ LowLevel.SD.sd_getexternalinfo sds)

sd_setblocksize :: forall a (t :: HDataType a) (n :: Nat) r s.
    SDataSetId n t 'HDFWrite (r,s) -> Int32 -> HDFio (r,s) ()
sd_setblocksize (SDataSetId sds) blockSize = check =<< (liftIO $ LowLevel.SD.sd_setblocksize sds blockSize)

sd_setexternalfile :: forall a (t :: HDataType a) (n :: Nat) r s.
    SDataSetId n t 'HDFWrite (r,s) -> String -> Int32 -> HDFio (r,s) ()
sd_setexternalfile (SDataSetId sds) fileName offset = check =<< (liftIO $ LowLevel.SD.sd_setexternalfile sds fileName offset)

sd_isdimval_bwcomp :: SDimensionId any s -> HDFio s Bool
sd_isdimval_bwcomp (SDimensionId dim) = check =<< (liftIO $ LowLevel.SD.sd_isdimval_bwcomp dim)

sd_setdimval_comp :: forall s.
    SDimensionId 'HDFWrite s -> Bool -> HDFio s ()
sd_setdimval_comp (SDimensionId dim) bwCompatible = check =<< (liftIO $ LowLevel.SD.sd_setdimval_comp dim bwCompatible)

sd_setdimscale :: forall a s. (Storable a) =>
    SDimensionId 'HDFWrite s -> HDataType a -> VS.Vector a -> HDFio s ()
sd_setdimscale (SDimensionId dim) dataType dimScale = check =<< (liftIO $ LowLevel.SD.sd_setdimscale dim dataType dimScale)

sd_getdimscale :: SDataSetId n t any (r,s) -> SDimensionId any s -> HDFio (r,s) HDFVector
sd_getdimscale (SDataSetId sds) (SDimensionId dim) = check =<< (liftIO $ LowLevel.SD.sd_getdimscale sds dim)

sd_setattr :: forall id (t :: Type) (a :: Type) s.
    (Internal.SDObjectId (LLId id), SDObjectId id, Storable a, CanBeAttribute t, a `IsElementOf` t) =>
    id 'HDFWrite s -> String -> HDataType a -> t -> HDFio s ()
sd_setattr obj attrName dataType attrValue = check =<< (liftIO $ LowLevel.SD.sd_setattr (getRawObjectId obj) attrName dataType attrValue)

sd_readattr :: (Internal.SDObjectId (LLId id), SDObjectId id) =>
    id any s -> Int32 -> HDFio s HDFVector
sd_readattr obj attrId = check =<< (liftIO $ LowLevel.SD.sd_readattr (getRawObjectId obj) attrId)

sd_writechunk :: forall a (t :: HDataType a) (n :: Nat) r s. Storable a =>
    SDataSetId n t 'HDFWrite (r,s) -> [Int32] -> VS.Vector a -> HDFio (r,s) ()
sd_writechunk (SDataSetId sds) chunkCoords dataChunk = check =<< (liftIO $ LowLevel.SD.sd_writechunk sds chunkCoords dataChunk)

sd_readchunk :: forall a (t :: HDataType a) (n :: Nat) any r s. Storable a =>
    SDataSetId n t any (r,s) -> [Int32] -> HDFio (r,s) (VS.Vector a)
sd_readchunk (SDataSetId sds) chunkCoords = check =<< (liftIO $ LowLevel.SD.sd_readchunk sds chunkCoords)

sd_writedata :: forall a (t :: HDataType a) (n :: Nat) r s. (Storable a, KnownNat n) =>
    SDataSetId n t 'HDFWrite (r,s) -> Index n -> Index n -> Index n -> VS.Vector a -> HDFio (r,s) ()
sd_writedata (SDataSetId sds) start stride edges sdsData = check =<< (liftIO $ LowLevel.SD.sd_writedata sds start stride edges sdsData)

sd_readdata :: forall a (t :: HDataType a) (n :: Nat) any r s. (Storable a, KnownNat n) =>
    SDataSetId n t any (r,s) -> Index n -> Index n -> Index n -> HDFio (r,s) (VS.Vector a)
sd_readdata (SDataSetId sds) start stride edges = check =<< (liftIO $ LowLevel.SD.sd_readdata sds start stride edges)
