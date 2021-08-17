{-# LANGUAGE GADTs #-}
module Data.Format.NetCDF.LowLevel.C.Definitions where

import qualified Data.Bits as B
import           Foreign.C.Types

import           Data.Format.NetCDF.LowLevel.Definitions
import           Internal.Definitions

#include <netcdf.h>

toNCOpenModeTag :: NCOpenMode -> CInt
toNCOpenModeTag NCNoWrite             = #{const NC_NOWRITE  }
toNCOpenModeTag NCWrite               = #{const NC_WRITE    }
toNCOpenModeTag NCShare               = #{const NC_SHARE    }
toNCOpenModeTag NCDiskless            = #{const NC_DISKLESS }
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
toNCOpenModeTag NCPersist             = #{const NC_PERSIST  }
#endif
toNCOpenModeTag NCClobber             = #{const NC_CLOBBER  }
toNCOpenModeTag NCNoClobber           = #{const NC_NOCLOBBER}
toNCOpenModeTag (NCCompoundMode mode) = mode

queryOpenMode :: NCOpenMode -> NCOpenMode -> Bool
queryOpenMode m1 (NCCompoundMode m2) = (toNCOpenModeTag m1 B..&. m2) /= 0
queryOpenMode m1 m2 = m1 == m2

-- It is not possible to distinguish NC_NOWRITE from NC_CLOBBER
-- because they are represented by the same value of 0. So, since
-- NC_CLOBBER is the default mode, NCNoWrite was given priority.
fromNCOpenModeTag :: CInt -> NCOpenMode
fromNCOpenModeTag tag = case tag of
    #{const NC_NOWRITE  } -> NCNoWrite
    #{const NC_WRITE    } -> NCWrite
    #{const NC_SHARE    } -> NCShare
    #{const NC_DISKLESS } -> NCDiskless
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
    #{const NC_PERSIST  } -> NCPersist
#endif
    #{const NC_NOCLOBBER} -> NCNoClobber
    mode                  -> NCCompoundMode mode

(.|.) :: NCOpenMode -> NCOpenMode -> NCOpenMode
(.|.) l r = NCCompoundMode $ (toNCOpenModeTag l) B..|. (toNCOpenModeTag r)

toNCFormatTag :: NCFormat -> CInt
toNCFormatTag NCClassic          = 0
toNCFormatTag NC64BitOffset      = #{const NC_64BIT_OFFSET}
toNCFormatTag NC64BitData        = #{const NC_64BIT_DATA}
toNCFormatTag NCNetCDF4          = #{const NC_NETCDF4}
toNCFormatTag (NCClassicModel f) = #{const NC_CLASSIC_MODEL} B..|. (toNCFormatTag f)

fromNCFormatTag :: CInt -> Maybe NCFormat
fromNCFormatTag 0 = Just NCClassic
fromNCFormatTag tag
  |     (tag B..&. #{const NC_64BIT_OFFSET} )   /= 0 = Just NC64BitOffset
  |     (tag B..&. #{const NC_64BIT_DATA}   )   /= 0 = Just NC64BitData
  |     (tag B..&. #{const NC_NETCDF4}      )   /= 0 = Just NCNetCDF4
  | s <- tag B..&. #{const NC_CLASSIC_MODEL}, s /= 0 =
    case fromNCFormatTag s of
        Just ncFormat -> Just (NCClassicModel ncFormat)
        Nothing -> Nothing
  | otherwise                                  = Nothing

fromNCInqFormatTag :: CInt -> Maybe NCFormat
fromNCInqFormatTag tag = case tag of
    #{const NC_FORMAT_CLASSIC}         -> Just NCClassic
    #{const NC_FORMAT_64BIT_OFFSET}    -> Just NC64BitOffset
    #{const NC_FORMAT_NETCDF4}         -> Just NCNetCDF4
    #{const NC_FORMAT_NETCDF4_CLASSIC} -> Just (NCClassicModel NCNetCDF4)
    #{const NC_FORMAT_64BIT_DATA}      -> Just NC64BitData
    _ -> Nothing

toNCFormatXTag :: NCFormatX -> CInt
toNCFormatXTag NCFormatXNC3       = #{const NC_FORMATX_NC3}
toNCFormatXTag NCFormatXNChdf5    = #{const NC_FORMATX_NC_HDF5}
toNCFormatXTag NCFormatXNChdf4    = #{const NC_FORMATX_NC_HDF4}
toNCFormatXTag NCFormatXPNetCDF   = #{const NC_FORMATX_PNETCDF}
toNCFormatXTag NCFormatXDAP2      = #{const NC_FORMATX_DAP2}
toNCFormatXTag NCFormatXDAP4      = #{const NC_FORMATX_DAP4}
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
toNCFormatXTag NCFormatXUDF0      = #{const NC_FORMATX_UDF0}
toNCFormatXTag NCFormatXUDF1      = #{const NC_FORMATX_UDF1}
#endif
#if   PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,7,0) && PKG_CONFIG_NETCDF_VERSION < PKG_VERSION(4,8,0)
toNCFormatXTag NCFormatXZARR      = #{const NC_FORMATX_ZARR}
#elif PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,8,0)
toNCFormatXTag NCFormatXZARR      = #{const NC_FORMATX_NCZARR}
#endif
toNCFormatXTag NCFormatXUndefined = #{const NC_FORMATX_UNDEFINED}

fromNCFormatXTag :: CInt -> Maybe NCFormatX
fromNCFormatXTag tag = case tag of
    #{const NC_FORMATX_NC3}       -> Just NCFormatXNC3
    #{const NC_FORMATX_NC_HDF5}   -> Just NCFormatXNChdf5
    #{const NC_FORMATX_NC_HDF4}   -> Just NCFormatXNChdf4
    #{const NC_FORMATX_PNETCDF}   -> Just NCFormatXPNetCDF
    #{const NC_FORMATX_DAP2}      -> Just NCFormatXDAP2
    #{const NC_FORMATX_DAP4}      -> Just NCFormatXDAP4
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
    #{const NC_FORMATX_UDF0}      -> Just NCFormatXUDF0
    #{const NC_FORMATX_UDF1}      -> Just NCFormatXUDF1
#endif
#if   PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,7,0) && PKG_CONFIG_NETCDF_VERSION < PKG_VERSION(4,8,0)
    #{const NC_FORMATX_ZARR}      -> Just NCFormatXZARR
#elif PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,8,0)
    #{const NC_FORMATX_NCZARR}    -> Just NCFormatXZARR
#endif
    #{const NC_FORMATX_UNDEFINED} -> Just NCFormatXUndefined
    _ -> Nothing

toNCFillTag :: NCFillMode -> CInt
toNCFillTag NCFill   = #{const NC_FILL}
toNCFillTag NCNoFill = #{const NC_NOFILL}

fromNCFillTag :: CInt -> Maybe NCFillMode
fromNCFillTag tag = case tag of
    #{const NC_FILL}   -> Just NCFill
    #{const NC_NOFILL} -> Just NCNoFill
    _ -> Nothing

toNCStorageTypeTag :: NCStorageType -> CInt
toNCStorageTypeTag NCContiguous = #{const NC_CONTIGUOUS}
toNCStorageTypeTag NCChunked    = #{const NC_CHUNKED   }

fromNCStorageTypeTag :: CInt -> Maybe NCStorageType
fromNCStorageTypeTag tag = case tag of
    #{const NC_CONTIGUOUS} -> Just NCContiguous
    #{const NC_CHUNKED   } -> Just NCChunked
    _ -> Nothing

toNCEndiannessTypeTag :: NCEndianness -> CInt
toNCEndiannessTypeTag NCEndianNative = #{const NC_ENDIAN_NATIVE}
toNCEndiannessTypeTag NCEndianLittle = #{const NC_ENDIAN_LITTLE}
toNCEndiannessTypeTag NCEndianBig    = #{const NC_ENDIAN_BIG   }

fromNCEndiannessTypeTag :: CInt -> Maybe NCEndianness
fromNCEndiannessTypeTag tag = case tag of
    #{const NC_ENDIAN_NATIVE} -> Just NCEndianNative
    #{const NC_ENDIAN_LITTLE} -> Just NCEndianLittle
    #{const NC_ENDIAN_BIG   } -> Just NCEndianBig
    _ -> Nothing

ncUnlimitedDimension :: CSize
ncUnlimitedDimension = #{const NC_UNLIMITED}

ncMaxNameLen :: CSize
ncMaxNameLen = #{const NC_MAX_NAME}

fromNCDataType :: NCDataType a -> CInt
fromNCDataType NCNone    = 0
fromNCDataType NCByte    = #{const NC_BYTE  }
fromNCDataType NCUByte   = #{const NC_UBYTE }
fromNCDataType NCChar    = #{const NC_CHAR  }
fromNCDataType NCShort   = #{const NC_SHORT }
fromNCDataType NCUShort  = #{const NC_USHORT}
fromNCDataType NCInt     = #{const NC_INT   }
fromNCDataType NCUInt    = #{const NC_UINT  }
fromNCDataType NCInt64   = #{const NC_INT64 }
fromNCDataType NCUInt64  = #{const NC_UINT64}
fromNCDataType NCFloat   = #{const NC_FLOAT }
fromNCDataType NCDouble  = #{const NC_DOUBLE}
fromNCDataType NCString  = #{const NC_STRING}

fromNCTypeTag :: CInt -> NCType
fromNCTypeTag tag = case tag of
    #{const NC_BYTE  } -> TypedValue NCByte   ()
    #{const NC_UBYTE } -> TypedValue NCUByte  ()
    #{const NC_CHAR  } -> TypedValue NCChar   ()
    #{const NC_SHORT } -> TypedValue NCShort  ()
    #{const NC_USHORT} -> TypedValue NCUShort ()
    #{const NC_INT   } -> TypedValue NCInt    ()
    #{const NC_UINT  } -> TypedValue NCUInt   ()
    #{const NC_INT64 } -> TypedValue NCInt64  ()
    #{const NC_UINT64} -> TypedValue NCUInt64 ()
    #{const NC_FLOAT } -> TypedValue NCFloat  ()
    #{const NC_DOUBLE} -> TypedValue NCDouble ()
    #{const NC_STRING} -> TypedValue NCString ()
    _                  -> TypedValue NCNone   ()

ncGlobalAttribute :: CInt
ncGlobalAttribute = #{const NC_GLOBAL}
