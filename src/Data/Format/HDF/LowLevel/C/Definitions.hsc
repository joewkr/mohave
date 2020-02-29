{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Data.Format.HDF.LowLevel.C.Definitions where

import           Data.Int
import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable

import           Data.Format.HDF.LowLevel.Definitions

#include <hdf.h>
#include <mfhdf.h>

toHDFOpenModeTag :: HDFOpenMode -> Int32
toHDFOpenModeTag HDFRead   = #{const DFACC_READ  }
toHDFOpenModeTag HDFWrite  = #{const DFACC_WRITE }
toHDFOpenModeTag HDFCreate = #{const DFACC_CREATE}

toHDFFillModeTag :: HDFFillMode -> Int32
toHDFFillModeTag HDFFill   = #{const SD_FILL     }
toHDFFillModeTag HDFNoFill = #{const SD_NOFILL   }

fromHDFFillModeTag :: Int32 -> Maybe HDFFillMode
fromHDFFillModeTag tag = case tag of
    #{const SD_FILL     } -> Just HDFFill
    #{const SD_NOFILL   } -> Just HDFNoFill
    _                     -> Nothing

fromHDFTypeTag :: Int32 -> HDFType
fromHDFTypeTag tag = case tag of
    #{const (DFNT_HDF    | DFNT_UINT8  )} -> HDFValue HWord8   ()
    #{const (DFNT_HDF    | DFNT_UINT16 )} -> HDFValue HWord16  ()
    #{const (DFNT_HDF    | DFNT_UINT32 )} -> HDFValue HWord32  ()
    #{const (DFNT_HDF    | DFNT_INT8   )} -> HDFValue HInt8    ()
    #{const (DFNT_HDF    | DFNT_INT16  )} -> HDFValue HInt16   ()
    #{const (DFNT_HDF    | DFNT_INT32  )} -> HDFValue HInt32   ()
    #{const (DFNT_HDF    | DFNT_FLOAT32)} -> HDFValue HFloat32 ()
    #{const (DFNT_HDF    | DFNT_FLOAT64)} -> HDFValue HFloat64 ()

    #{const (DFNT_HDF    | DFNT_CHAR8  )} -> HDFValue HChar8   ()
    #{const (DFNT_HDF    | DFNT_UCHAR8 )} -> HDFValue HUChar8  ()

    #{const (DFNT_NATIVE | DFNT_UINT8  )} -> HDFValue HWord8   ()
    #{const (DFNT_NATIVE | DFNT_UINT16 )} -> HDFValue HWord16  ()
    #{const (DFNT_NATIVE | DFNT_UINT32 )} -> HDFValue HWord32  ()
    #{const (DFNT_NATIVE | DFNT_INT8   )} -> HDFValue HInt8    ()
    #{const (DFNT_NATIVE | DFNT_INT16  )} -> HDFValue HInt16   ()
    #{const (DFNT_NATIVE | DFNT_INT32  )} -> HDFValue HInt32   ()
    #{const (DFNT_NATIVE | DFNT_FLOAT32)} -> HDFValue HFloat32 ()
    #{const (DFNT_NATIVE | DFNT_FLOAT64)} -> HDFValue HFloat64 ()

    #{const (DFNT_NATIVE | DFNT_CHAR8  )} -> HDFValue HChar8   ()
    #{const (DFNT_NATIVE | DFNT_UCHAR8 )} -> HDFValue HUChar8  ()

    #{const (DFNT_CUSTOM | DFNT_UINT8  )} -> HDFValue HWord8   ()
    #{const (DFNT_CUSTOM | DFNT_UINT16 )} -> HDFValue HWord16  ()
    #{const (DFNT_CUSTOM | DFNT_UINT32 )} -> HDFValue HWord32  ()
    #{const (DFNT_CUSTOM | DFNT_INT8   )} -> HDFValue HInt8    ()
    #{const (DFNT_CUSTOM | DFNT_INT16  )} -> HDFValue HInt16   ()
    #{const (DFNT_CUSTOM | DFNT_INT32  )} -> HDFValue HInt32   ()
    #{const (DFNT_CUSTOM | DFNT_FLOAT32)} -> HDFValue HFloat32 ()
    #{const (DFNT_CUSTOM | DFNT_FLOAT64)} -> HDFValue HFloat64 ()

    #{const (DFNT_CUSTOM | DFNT_CHAR8  )} -> HDFValue HChar8   ()
    #{const (DFNT_CUSTOM | DFNT_UCHAR8 )} -> HDFValue HUChar8  ()

    #{const (DFNT_LITEND | DFNT_UINT8  )} -> HDFValue HWord8   ()
    #{const (DFNT_LITEND | DFNT_UINT16 )} -> HDFValue HWord16  ()
    #{const (DFNT_LITEND | DFNT_UINT32 )} -> HDFValue HWord32  ()
    #{const (DFNT_LITEND | DFNT_INT8   )} -> HDFValue HInt8    ()
    #{const (DFNT_LITEND | DFNT_INT16  )} -> HDFValue HInt16   ()
    #{const (DFNT_LITEND | DFNT_INT32  )} -> HDFValue HInt32   ()
    #{const (DFNT_LITEND | DFNT_FLOAT32)} -> HDFValue HFloat32 ()
    #{const (DFNT_LITEND | DFNT_FLOAT64)} -> HDFValue HFloat64 ()

    #{const (DFNT_LITEND | DFNT_CHAR8  )} -> HDFValue HChar8   ()
    #{const (DFNT_LITEND | DFNT_UCHAR8 )} -> HDFValue HUChar8  ()

    _                     -> HDFValue HNone    ()

toHDFTypeTag :: HDFType -> Int32
toHDFTypeTag (HDFValue t _) = fromHDataType t

fromHDataType :: HDataType a -> Int32
fromHDataType HNone    = 0
fromHDataType HChar8   = #{const DFNT_CHAR8  }
fromHDataType HUChar8  = #{const DFNT_UCHAR8 }
fromHDataType HWord8   = #{const DFNT_UINT8  }
fromHDataType HWord16  = #{const DFNT_UINT16 }
fromHDataType HWord32  = #{const DFNT_UINT32 }
fromHDataType HInt8    = #{const DFNT_INT8   }
fromHDataType HInt16   = #{const DFNT_INT16  }
fromHDataType HInt32   = #{const DFNT_INT32  }
fromHDataType HFloat32 = #{const DFNT_FLOAT32}
fromHDataType HFloat64 = #{const DFNT_FLOAT64}

hdfMaxVarDims :: Int32
hdfMaxVarDims = #const MAX_VAR_DIMS

hdfMaxNcNameLen :: Int32
hdfMaxNcNameLen = #const MAX_NC_NAME


data HDFVarList = HDFVarList {
    hdf_var_index :: Int32
  , hdf_var_type  :: #{type hdf_vartype_t}
} deriving (Eq, Show)

instance Storable HDFVarList where
  alignment _ = #{alignment hdf_varlist_t}
  sizeOf _ = #{size hdf_varlist_t}
  peek ptr = do
    var_index <- #{peek hdf_varlist_t, var_index} ptr
    var_type  <- #{peek hdf_varlist_t, var_type} ptr
    return $! HDFVarList var_index var_type
  poke ptr (HDFVarList var_index var_type) = do
    #{poke hdf_varlist_t, var_index} ptr var_index
    #{poke hdf_varlist_t, var_type}  ptr var_type

data HDFCompParams =
    HDFCompNone
  | HDFCompRLE
  | HDFCompNBit {
      hdfCompRawType :: HDFType
    , hdfCompSignExt :: CInt
    , hdfCompFillOne :: CInt
    , hdfCompStartBit :: CInt
    , hdfCompBitLen :: CInt}
  | HDFCompSkHuff {hdfCompSkpSize :: CInt}
  | HDFCompDeflate {hdfCompLevel :: CInt}
  | HDFCompSZip {
      hdfCompOptionsMask :: Int32
    , hdfCompPixelsPerBlock :: Int32
    , hdfCompPixelsPerScanline :: Int32
    , hdfCompBitsPerPixel :: Int32
    , hdfCompPixels :: Int32}
  deriving (Show, Eq)

class TaggedData a where
  rawDataSize :: a -> Int

tagAlignement, tagSize :: Int
tagAlignement = alignment (undefined :: #{type comp_coder_t})
tagSize = sizeOf (undefined :: #{type comp_coder_t})

tagPtr :: forall a.TaggedData a => Ptr a -> Ptr #{type comp_coder_t}
tagPtr taggedPtr =
  (flip alignPtr) tagAlignement . plusPtr taggedPtr $ rawDataSize (undefined :: a)

embedCompTag :: TaggedData a => Ptr a -> #{type comp_coder_t} -> IO ()
embedCompTag compParamsPtr tag = poke (tagPtr compParamsPtr) tag

instance TaggedData HDFCompParams where
  rawDataSize _ = #{size comp_info}

instance Storable HDFCompParams where
  alignment _ = #{alignment comp_info}
  sizeOf _ =
    ((#{size comp_info} + #{alignment comp_info} + tagAlignement - 1)
    `div` tagAlignement)*tagSize - #{alignment comp_info} + tagSize
  peek ptr = do
    tag <- peek (tagPtr ptr)
    case tag of
      #{const COMP_CODE_NONE} -> do
        return $! HDFCompNone
      #{const COMP_CODE_RLE} -> do
        return $! HDFCompRLE
      #{const COMP_CODE_NBIT} -> do
        rawType  <- #{peek comp_info, nbit.nt} ptr
        signExt  <- #{peek comp_info, nbit.sign_ext} ptr
        fillOne  <- #{peek comp_info, nbit.fill_one} ptr
        startBit <- #{peek comp_info, nbit.start_bit} ptr
        bitLen   <- #{peek comp_info, nbit.bit_len} ptr
        return $! HDFCompNBit
          (fromHDFTypeTag rawType)
          signExt
          fillOne
          startBit
          bitLen
      #{const COMP_CODE_SKPHUFF} -> do
        skp_size <- #{peek comp_info, skphuff.skp_size} ptr
        return $! HDFCompSkHuff skp_size
      #{const COMP_CODE_DEFLATE} -> do
        comp_level <- #{peek comp_info, deflate.level} ptr
        return $! HDFCompDeflate comp_level
      #{const COMP_CODE_SZIP} -> do
        options_mask        <- #{peek comp_info, szip.options_mask} ptr
        pixels_per_block    <- #{peek comp_info, szip.pixels_per_block} ptr
        pixels_per_scanline <- #{peek comp_info, szip.pixels_per_scanline} ptr
        bits_per_pixel      <- #{peek comp_info, szip.bits_per_pixel} ptr
        pixels              <- #{peek comp_info, szip.pixels} ptr
        return $! HDFCompSZip
          options_mask
          pixels_per_block
          pixels_per_scanline
          bits_per_pixel
          pixels
      _ -> error "HDFCompParams: Undefined tag"
  poke _ HDFCompNone = return ()
  poke _ HDFCompRLE = return ()
  poke ptr
    (HDFCompNBit
      rawType
      signExt
      fillOne
      startBit
      bitLen) = do
        #{poke comp_info, nbit.nt} ptr (toHDFTypeTag rawType)
        #{poke comp_info, nbit.sign_ext} ptr signExt
        #{poke comp_info, nbit.fill_one} ptr fillOne
        #{poke comp_info, nbit.start_bit} ptr startBit
        #{poke comp_info, nbit.bit_len} ptr bitLen
  poke ptr (HDFCompSkHuff skp_size) = do
    #{poke comp_info, skphuff.skp_size} ptr skp_size
  poke ptr (HDFCompDeflate comp_level) = do
    #{poke comp_info, deflate.level} ptr comp_level
  poke ptr
    (HDFCompSZip
      options_mask
      pixels_per_block
      pixels_per_scanline
      bits_per_pixel
      pixels) = do
        #{poke comp_info, szip.options_mask} ptr options_mask
        #{poke comp_info, szip.pixels_per_block} ptr pixels_per_block
        #{poke comp_info, szip.pixels_per_scanline} ptr pixels_per_scanline
        #{poke comp_info, szip.bits_per_pixel} ptr bits_per_pixel
        #{poke comp_info, szip.pixels} ptr pixels

type HDFCompType = #{type comp_coder_t}

toHDFCompModeTag :: HDFCompParams -> HDFCompType
toHDFCompModeTag HDFCompNone{}    = #{const COMP_CODE_NONE   }
toHDFCompModeTag HDFCompRLE{}     = #{const COMP_CODE_RLE    }
toHDFCompModeTag HDFCompNBit{}    = #{const COMP_CODE_NBIT   }
toHDFCompModeTag HDFCompSkHuff{}  = #{const COMP_CODE_SKPHUFF}
toHDFCompModeTag HDFCompDeflate{} = #{const COMP_CODE_DEFLATE}
toHDFCompModeTag HDFCompSZip{}    = #{const COMP_CODE_SZIP   }

data HDFChunkParams = HDFChunkParams {
    hdfChunkSizes       :: [Int32]
  , hdfChunkCompression :: HDFCompParams}
  deriving (Show, Eq)

instance TaggedData HDFChunkParams where
  rawDataSize _ = #{size HDF_CHUNK_DEF}

instance Storable HDFChunkParams where
  alignment _ = #{alignment HDF_CHUNK_DEF}
  sizeOf _ =
    ((#{size HDF_CHUNK_DEF} + #{alignment HDF_CHUNK_DEF} + tagAlignement - 1)
    `div` tagAlignement)*tagSize - #{alignment HDF_CHUNK_DEF} + tagSize
  peek ptr = do
    tag <- peek (tagPtr ptr)
    case tag of
      #{const HDF_NONE} -> do
        return $! HDFChunkParams [] HDFCompNone
      #{const HDF_CHUNK} -> do
        let chunksPtr = plusPtr ptr #{offset HDF_CHUNK_DEF, chunk_lengths}
        chunks <- filter (/= 0) <$> peekArray #{const H4_MAX_VAR_DIMS} chunksPtr
        return $! HDFChunkParams chunks HDFCompNone
      #{const HDF_CHUNK|HDF_COMP} -> do
        let chunksPtr = plusPtr ptr #{offset HDF_CHUNK_DEF, comp.chunk_lengths}
        chunks <- filter (/= 0) <$> peekArray #{const H4_MAX_VAR_DIMS} chunksPtr
        -- We need to embed tag to be able to peek HDFCompParams, so
        -- allocate memory for new HDFCompParams, copy raw data from
        -- the input pointer and attach the tag.
        compParams <- alloca $ \compParamsPtr -> do
          let
            cinfoPtr = #{ptr HDF_CHUNK_DEF, comp.cinfo} ptr
            cinfoSize = rawDataSize (undefined :: HDFCompParams)
          copyBytes compParamsPtr cinfoPtr cinfoSize
          #{peek HDF_CHUNK_DEF, comp.comp_type} ptr >>= embedCompTag compParamsPtr
          peek compParamsPtr
        return $! HDFChunkParams chunks compParams
      #{const HDF_CHUNK|HDF_NBIT} -> do
        let chunksPtr = plusPtr ptr #{offset HDF_CHUNK_DEF, nbit.chunk_lengths}
        chunks <- filter (/= 0) <$> peekArray #{const H4_MAX_VAR_DIMS} chunksPtr
        signExt  <- #{peek HDF_CHUNK_DEF, nbit.sign_ext} ptr
        fillOne  <- #{peek HDF_CHUNK_DEF, nbit.fill_one} ptr
        startBit <- #{peek HDF_CHUNK_DEF, nbit.start_bit} ptr
        bitLen   <- #{peek HDF_CHUNK_DEF, nbit.bit_len} ptr
        return $! HDFChunkParams chunks $
          HDFCompNBit
            (HDFValue HNone ())
            signExt
            fillOne
            startBit
            bitLen
      _ -> error "HDFChunkParams: Undefined tag"
  poke ptr (HDFChunkParams chunks compParams) = case compParams of
    HDFCompNone -> do
      let chunksPtr = plusPtr ptr #{offset HDF_CHUNK_DEF, chunk_lengths}
      pokeArray chunksPtr chunksPadded
    (HDFCompNBit _ signExt fillOne startBit bitLen) -> do
      let chunksPtr = plusPtr ptr #{offset HDF_CHUNK_DEF, nbit.chunk_lengths}
      pokeArray chunksPtr chunksPadded
      #{poke HDF_CHUNK_DEF, nbit.sign_ext} ptr signExt
      #{poke HDF_CHUNK_DEF, nbit.fill_one} ptr fillOne
      #{poke HDF_CHUNK_DEF, nbit.start_bit} ptr startBit
      #{poke HDF_CHUNK_DEF, nbit.bit_len} ptr bitLen
    _ -> do
      let
        chunksPtr = plusPtr ptr #{offset HDF_CHUNK_DEF, comp.chunk_lengths}
        compTag = (fromIntegral . toHDFCompModeTag $ compParams) :: Int32
      pokeArray chunksPtr chunksPadded
      #{poke HDF_CHUNK_DEF, comp.comp_type} ptr compTag
      #{poke HDF_CHUNK_DEF, comp.cinfo} ptr compParams
    where
      chunksPadded = take #{const H4_MAX_VAR_DIMS} $ chunks ++ repeat 0

selectChunkingMode :: HDFChunkParams -> Int32
selectChunkingMode (HDFChunkParams _ HDFCompNone  ) = #{const HDF_CHUNK}
selectChunkingMode (HDFChunkParams _ HDFCompNBit{}) = #{const HDF_CHUNK | HDF_NBIT}
selectChunkingMode (HDFChunkParams _ _            ) = #{const HDF_CHUNK | HDF_COMP}

type CAnnType = #{type ann_type}

toHDFAnnotationTypeTag :: HDFAnnotationType -> CAnnType
toHDFAnnotationTypeTag HDFAnnUndef     = #{const AN_UNDEF     }
toHDFAnnotationTypeTag HDFAnnDataLabel = #{const AN_DATA_LABEL}
toHDFAnnotationTypeTag HDFAnnDataDesc  = #{const AN_DATA_DESC }
toHDFAnnotationTypeTag HDFAnnFileLabel = #{const AN_FILE_LABEL}
toHDFAnnotationTypeTag HDFAnnFileDesc  = #{const AN_FILE_DESC }

type HDFErrorCode = #{type hdf_err_code_t}

fromHDFErrorCode :: HDFErrorCode -> HDFError
fromHDFErrorCode e = case e of
  #{const DFE_NONE          } -> DFE_NONE
  #{const DFE_FNF           } -> DFE_FNF
  #{const DFE_DENIED        } -> DFE_DENIED
  #{const DFE_ALROPEN       } -> DFE_ALROPEN
  #{const DFE_TOOMANY       } -> DFE_TOOMANY
  #{const DFE_BADNAME       } -> DFE_BADNAME
  #{const DFE_BADACC        } -> DFE_BADACC
  #{const DFE_BADOPEN       } -> DFE_BADOPEN
  #{const DFE_NOTOPEN       } -> DFE_NOTOPEN
  #{const DFE_CANTCLOSE     } -> DFE_CANTCLOSE
  #{const DFE_READERROR     } -> DFE_READERROR
  #{const DFE_WRITEERROR    } -> DFE_WRITEERROR
  #{const DFE_SEEKERROR     } -> DFE_SEEKERROR
  #{const DFE_RDONLY        } -> DFE_RDONLY
  #{const DFE_BADSEEK       } -> DFE_BADSEEK
  #{const DFE_INVFILE       } -> DFE_INVFILE
  #{const DFE_PUTELEM       } -> DFE_PUTELEM
  #{const DFE_GETELEM       } -> DFE_GETELEM
  #{const DFE_CANTLINK      } -> DFE_CANTLINK
  #{const DFE_CANTSYNC      } -> DFE_CANTSYNC
  #{const DFE_BADGROUP      } -> DFE_BADGROUP
  #{const DFE_GROUPSETUP    } -> DFE_GROUPSETUP
  #{const DFE_PUTGROUP      } -> DFE_PUTGROUP
  #{const DFE_GROUPWRITE    } -> DFE_GROUPWRITE
  #{const DFE_DFNULL        } -> DFE_DFNULL
  #{const DFE_ILLTYPE       } -> DFE_ILLTYPE
  #{const DFE_BADDDLIST     } -> DFE_BADDDLIST
  #{const DFE_NOTDFFILE     } -> DFE_NOTDFFILE
  #{const DFE_SEEDTWICE     } -> DFE_SEEDTWICE
  #{const DFE_NOSUCHTAG     } -> DFE_NOSUCHTAG
  #{const DFE_NOFREEDD      } -> DFE_NOFREEDD
  #{const DFE_BADTAG        } -> DFE_BADTAG
  #{const DFE_BADREF        } -> DFE_BADREF
  #{const DFE_NOMATCH       } -> DFE_NOMATCH
  #{const DFE_NOTINSET      } -> DFE_NOTINSET
  #{const DFE_BADOFFSET     } -> DFE_BADOFFSET
  #{const DFE_CORRUPT       } -> DFE_CORRUPT
  #{const DFE_NOREF         } -> DFE_NOREF
  #{const DFE_DUPDD         } -> DFE_DUPDD
  #{const DFE_CANTMOD       } -> DFE_CANTMOD
  #{const DFE_DIFFFILES     } -> DFE_DIFFFILES
  #{const DFE_BADAID        } -> DFE_BADAID
  #{const DFE_OPENAID       } -> DFE_OPENAID
  #{const DFE_CANTFLUSH     } -> DFE_CANTFLUSH
  #{const DFE_CANTUPDATE    } -> DFE_CANTUPDATE
  #{const DFE_CANTHASH      } -> DFE_CANTHASH
  #{const DFE_CANTDELDD     } -> DFE_CANTDELDD
  #{const DFE_CANTDELHASH   } -> DFE_CANTDELHASH
  #{const DFE_CANTACCESS    } -> DFE_CANTACCESS
  #{const DFE_CANTENDACCESS } -> DFE_CANTENDACCESS
  #{const DFE_TABLEFULL     } -> DFE_TABLEFULL
  #{const DFE_NOTINTABLE    } -> DFE_NOTINTABLE
  #{const DFE_UNSUPPORTED   } -> DFE_UNSUPPORTED
  #{const DFE_NOSPACE       } -> DFE_NOSPACE
  #{const DFE_BADCALL       } -> DFE_BADCALL
  #{const DFE_BADPTR        } -> DFE_BADPTR
  #{const DFE_BADLEN        } -> DFE_BADLEN
  #{const DFE_NOTENOUGH     } -> DFE_NOTENOUGH
  #{const DFE_NOVALS        } -> DFE_NOVALS
  #{const DFE_ARGS          } -> DFE_ARGS
  #{const DFE_INTERNAL      } -> DFE_INTERNAL
  #{const DFE_NORESET       } -> DFE_NORESET
  #{const DFE_EXCEEDMAX     } -> DFE_EXCEEDMAX
  #{const DFE_GENAPP        } -> DFE_GENAPP
  #{const DFE_UNINIT        } -> DFE_UNINIT
  #{const DFE_CANTINIT      } -> DFE_CANTINIT
  #{const DFE_CANTSHUTDOWN  } -> DFE_CANTSHUTDOWN
  #{const DFE_BADDIM        } -> DFE_BADDIM
  #{const DFE_BADFP         } -> DFE_BADFP
  #{const DFE_BADDATATYPE   } -> DFE_BADDATATYPE
  #{const DFE_BADMCTYPE     } -> DFE_BADMCTYPE
  #{const DFE_BADNUMTYPE    } -> DFE_BADNUMTYPE
  #{const DFE_BADORDER      } -> DFE_BADORDER
  #{const DFE_RANGE         } -> DFE_RANGE
  #{const DFE_BADCONV       } -> DFE_BADCONV
  #{const DFE_BADTYPE       } -> DFE_BADTYPE
  #{const DFE_BADDIMNAME    } -> DFE_BADDIMNAME
  #{const DFE_NOVGREP       } -> DFE_NOVGREP
  #{const DFE_BADSCHEME     } -> DFE_BADSCHEME
  #{const DFE_BADMODEL      } -> DFE_BADMODEL
  #{const DFE_BADCODER      } -> DFE_BADCODER
  #{const DFE_MODEL         } -> DFE_MODEL
  #{const DFE_CODER         } -> DFE_CODER
  #{const DFE_CINIT         } -> DFE_CINIT
  #{const DFE_CDECODE       } -> DFE_CDECODE
  #{const DFE_CENCODE       } -> DFE_CENCODE
  #{const DFE_CTERM         } -> DFE_CTERM
  #{const DFE_CSEEK         } -> DFE_CSEEK
  #{const DFE_MINIT         } -> DFE_MINIT
  #{const DFE_COMPINFO      } -> DFE_COMPINFO
  #{const DFE_CANTCOMP      } -> DFE_CANTCOMP
  #{const DFE_CANTDECOMP    } -> DFE_CANTDECOMP
  #{const DFE_NOENCODER     } -> DFE_NOENCODER
  #{const DFE_NOSZLIB       } -> DFE_NOSZLIB
  #{const DFE_COMPVERSION   } -> DFE_COMPVERSION
  #{const DFE_READCOMP      } -> DFE_READCOMP
  #{const DFE_NODIM         } -> DFE_NODIM
  #{const DFE_BADRIG        } -> DFE_BADRIG
  #{const DFE_RINOTFOUND    } -> DFE_RINOTFOUND
  #{const DFE_BADATTR       } -> DFE_BADATTR
  #{const DFE_LUTNOTFOUND   } -> DFE_LUTNOTFOUND
  #{const DFE_GRNOTFOUND    } -> DFE_GRNOTFOUND
  #{const DFE_BADTABLE      } -> DFE_BADTABLE
  #{const DFE_BADSDG        } -> DFE_BADSDG
  #{const DFE_BADNDG        } -> DFE_BADNDG
  #{const DFE_VGSIZE        } -> DFE_VGSIZE
  #{const DFE_VTAB          } -> DFE_VTAB
  #{const DFE_CANTADDELEM   } -> DFE_CANTADDELEM
  #{const DFE_BADVGNAME     } -> DFE_BADVGNAME
  #{const DFE_BADVGCLASS    } -> DFE_BADVGCLASS
  #{const DFE_BADFIELDS     } -> DFE_BADFIELDS
  #{const DFE_NOVS          } -> DFE_NOVS
  #{const DFE_SYMSIZE       } -> DFE_SYMSIZE
  #{const DFE_BADATTACH     } -> DFE_BADATTACH
  #{const DFE_BADVSNAME     } -> DFE_BADVSNAME
  #{const DFE_BADVSCLASS    } -> DFE_BADVSCLASS
  #{const DFE_VSWRITE       } -> DFE_VSWRITE
  #{const DFE_VSREAD        } -> DFE_VSREAD
  #{const DFE_BADVH         } -> DFE_BADVH
  #{const DFE_FIELDSSET     } -> DFE_FIELDSSET
  #{const DFE_VSCANTCREATE  } -> DFE_VSCANTCREATE
  #{const DFE_VGCANTCREATE  } -> DFE_VGCANTCREATE
  #{const DFE_CANTATTACH    } -> DFE_CANTATTACH
  #{const DFE_CANTDETACH    } -> DFE_CANTDETACH
  #{const DFE_XDRERROR      } -> DFE_XDRERROR
  #{const DFE_BITREAD       } -> DFE_BITREAD
  #{const DFE_BITWRITE      } -> DFE_BITWRITE
  #{const DFE_BITSEEK       } -> DFE_BITSEEK
  #{const DFE_TBBTINS       } -> DFE_TBBTINS
  #{const DFE_BVNEW         } -> DFE_BVNEW
  #{const DFE_BVSET         } -> DFE_BVSET
  #{const DFE_BVGET         } -> DFE_BVGET
  #{const DFE_BVFIND        } -> DFE_BVFIND
  #{const DFE_CANTSETATTR   } -> DFE_CANTSETATTR
  #{const DFE_CANTGETATTR   } -> DFE_CANTGETATTR
  #{const DFE_ANAPIERROR    } -> DFE_ANAPIERROR
  _                           -> DFE_UNKNOWN_ERROR (fromIntegral e)

toHDFErrorCode :: HDFError -> HDFErrorCode
toHDFErrorCode  DFE_NONE             = #{const DFE_NONE          }
toHDFErrorCode  DFE_FNF              = #{const DFE_FNF           }
toHDFErrorCode  DFE_DENIED           = #{const DFE_DENIED        }
toHDFErrorCode  DFE_ALROPEN          = #{const DFE_ALROPEN       }
toHDFErrorCode  DFE_TOOMANY          = #{const DFE_TOOMANY       }
toHDFErrorCode  DFE_BADNAME          = #{const DFE_BADNAME       }
toHDFErrorCode  DFE_BADACC           = #{const DFE_BADACC        }
toHDFErrorCode  DFE_BADOPEN          = #{const DFE_BADOPEN       }
toHDFErrorCode  DFE_NOTOPEN          = #{const DFE_NOTOPEN       }
toHDFErrorCode  DFE_CANTCLOSE        = #{const DFE_CANTCLOSE     }
toHDFErrorCode  DFE_READERROR        = #{const DFE_READERROR     }
toHDFErrorCode  DFE_WRITEERROR       = #{const DFE_WRITEERROR    }
toHDFErrorCode  DFE_SEEKERROR        = #{const DFE_SEEKERROR     }
toHDFErrorCode  DFE_RDONLY           = #{const DFE_RDONLY        }
toHDFErrorCode  DFE_BADSEEK          = #{const DFE_BADSEEK       }
toHDFErrorCode  DFE_INVFILE          = #{const DFE_INVFILE       }
toHDFErrorCode  DFE_PUTELEM          = #{const DFE_PUTELEM       }
toHDFErrorCode  DFE_GETELEM          = #{const DFE_GETELEM       }
toHDFErrorCode  DFE_CANTLINK         = #{const DFE_CANTLINK      }
toHDFErrorCode  DFE_CANTSYNC         = #{const DFE_CANTSYNC      }
toHDFErrorCode  DFE_BADGROUP         = #{const DFE_BADGROUP      }
toHDFErrorCode  DFE_GROUPSETUP       = #{const DFE_GROUPSETUP    }
toHDFErrorCode  DFE_PUTGROUP         = #{const DFE_PUTGROUP      }
toHDFErrorCode  DFE_GROUPWRITE       = #{const DFE_GROUPWRITE    }
toHDFErrorCode  DFE_DFNULL           = #{const DFE_DFNULL        }
toHDFErrorCode  DFE_ILLTYPE          = #{const DFE_ILLTYPE       }
toHDFErrorCode  DFE_BADDDLIST        = #{const DFE_BADDDLIST     }
toHDFErrorCode  DFE_NOTDFFILE        = #{const DFE_NOTDFFILE     }
toHDFErrorCode  DFE_SEEDTWICE        = #{const DFE_SEEDTWICE     }
toHDFErrorCode  DFE_NOSUCHTAG        = #{const DFE_NOSUCHTAG     }
toHDFErrorCode  DFE_NOFREEDD         = #{const DFE_NOFREEDD      }
toHDFErrorCode  DFE_BADTAG           = #{const DFE_BADTAG        }
toHDFErrorCode  DFE_BADREF           = #{const DFE_BADREF        }
toHDFErrorCode  DFE_NOMATCH          = #{const DFE_NOMATCH       }
toHDFErrorCode  DFE_NOTINSET         = #{const DFE_NOTINSET      }
toHDFErrorCode  DFE_BADOFFSET        = #{const DFE_BADOFFSET     }
toHDFErrorCode  DFE_CORRUPT          = #{const DFE_CORRUPT       }
toHDFErrorCode  DFE_NOREF            = #{const DFE_NOREF         }
toHDFErrorCode  DFE_DUPDD            = #{const DFE_DUPDD         }
toHDFErrorCode  DFE_CANTMOD          = #{const DFE_CANTMOD       }
toHDFErrorCode  DFE_DIFFFILES        = #{const DFE_DIFFFILES     }
toHDFErrorCode  DFE_BADAID           = #{const DFE_BADAID        }
toHDFErrorCode  DFE_OPENAID          = #{const DFE_OPENAID       }
toHDFErrorCode  DFE_CANTFLUSH        = #{const DFE_CANTFLUSH     }
toHDFErrorCode  DFE_CANTUPDATE       = #{const DFE_CANTUPDATE    }
toHDFErrorCode  DFE_CANTHASH         = #{const DFE_CANTHASH      }
toHDFErrorCode  DFE_CANTDELDD        = #{const DFE_CANTDELDD     }
toHDFErrorCode  DFE_CANTDELHASH      = #{const DFE_CANTDELHASH   }
toHDFErrorCode  DFE_CANTACCESS       = #{const DFE_CANTACCESS    }
toHDFErrorCode  DFE_CANTENDACCESS    = #{const DFE_CANTENDACCESS }
toHDFErrorCode  DFE_TABLEFULL        = #{const DFE_TABLEFULL     }
toHDFErrorCode  DFE_NOTINTABLE       = #{const DFE_NOTINTABLE    }
toHDFErrorCode  DFE_UNSUPPORTED      = #{const DFE_UNSUPPORTED   }
toHDFErrorCode  DFE_NOSPACE          = #{const DFE_NOSPACE       }
toHDFErrorCode  DFE_BADCALL          = #{const DFE_BADCALL       }
toHDFErrorCode  DFE_BADPTR           = #{const DFE_BADPTR        }
toHDFErrorCode  DFE_BADLEN           = #{const DFE_BADLEN        }
toHDFErrorCode  DFE_NOTENOUGH        = #{const DFE_NOTENOUGH     }
toHDFErrorCode  DFE_NOVALS           = #{const DFE_NOVALS        }
toHDFErrorCode  DFE_ARGS             = #{const DFE_ARGS          }
toHDFErrorCode  DFE_INTERNAL         = #{const DFE_INTERNAL      }
toHDFErrorCode  DFE_NORESET          = #{const DFE_NORESET       }
toHDFErrorCode  DFE_EXCEEDMAX        = #{const DFE_EXCEEDMAX     }
toHDFErrorCode  DFE_GENAPP           = #{const DFE_GENAPP        }
toHDFErrorCode  DFE_UNINIT           = #{const DFE_UNINIT        }
toHDFErrorCode  DFE_CANTINIT         = #{const DFE_CANTINIT      }
toHDFErrorCode  DFE_CANTSHUTDOWN     = #{const DFE_CANTSHUTDOWN  }
toHDFErrorCode  DFE_BADDIM           = #{const DFE_BADDIM        }
toHDFErrorCode  DFE_BADFP            = #{const DFE_BADFP         }
toHDFErrorCode  DFE_BADDATATYPE      = #{const DFE_BADDATATYPE   }
toHDFErrorCode  DFE_BADMCTYPE        = #{const DFE_BADMCTYPE     }
toHDFErrorCode  DFE_BADNUMTYPE       = #{const DFE_BADNUMTYPE    }
toHDFErrorCode  DFE_BADORDER         = #{const DFE_BADORDER      }
toHDFErrorCode  DFE_RANGE            = #{const DFE_RANGE         }
toHDFErrorCode  DFE_BADCONV          = #{const DFE_BADCONV       }
toHDFErrorCode  DFE_BADTYPE          = #{const DFE_BADTYPE       }
toHDFErrorCode  DFE_BADDIMNAME       = #{const DFE_BADDIMNAME    }
toHDFErrorCode  DFE_NOVGREP          = #{const DFE_NOVGREP       }
toHDFErrorCode  DFE_BADSCHEME        = #{const DFE_BADSCHEME     }
toHDFErrorCode  DFE_BADMODEL         = #{const DFE_BADMODEL      }
toHDFErrorCode  DFE_BADCODER         = #{const DFE_BADCODER      }
toHDFErrorCode  DFE_MODEL            = #{const DFE_MODEL         }
toHDFErrorCode  DFE_CODER            = #{const DFE_CODER         }
toHDFErrorCode  DFE_CINIT            = #{const DFE_CINIT         }
toHDFErrorCode  DFE_CDECODE          = #{const DFE_CDECODE       }
toHDFErrorCode  DFE_CENCODE          = #{const DFE_CENCODE       }
toHDFErrorCode  DFE_CTERM            = #{const DFE_CTERM         }
toHDFErrorCode  DFE_CSEEK            = #{const DFE_CSEEK         }
toHDFErrorCode  DFE_MINIT            = #{const DFE_MINIT         }
toHDFErrorCode  DFE_COMPINFO         = #{const DFE_COMPINFO      }
toHDFErrorCode  DFE_CANTCOMP         = #{const DFE_CANTCOMP      }
toHDFErrorCode  DFE_CANTDECOMP       = #{const DFE_CANTDECOMP    }
toHDFErrorCode  DFE_NOENCODER        = #{const DFE_NOENCODER     }
toHDFErrorCode  DFE_NOSZLIB          = #{const DFE_NOSZLIB       }
toHDFErrorCode  DFE_COMPVERSION      = #{const DFE_COMPVERSION   }
toHDFErrorCode  DFE_READCOMP         = #{const DFE_READCOMP      }
toHDFErrorCode  DFE_NODIM            = #{const DFE_NODIM         }
toHDFErrorCode  DFE_BADRIG           = #{const DFE_BADRIG        }
toHDFErrorCode  DFE_RINOTFOUND       = #{const DFE_RINOTFOUND    }
toHDFErrorCode  DFE_BADATTR          = #{const DFE_BADATTR       }
toHDFErrorCode  DFE_LUTNOTFOUND      = #{const DFE_LUTNOTFOUND   }
toHDFErrorCode  DFE_GRNOTFOUND       = #{const DFE_GRNOTFOUND    }
toHDFErrorCode  DFE_BADTABLE         = #{const DFE_BADTABLE      }
toHDFErrorCode  DFE_BADSDG           = #{const DFE_BADSDG        }
toHDFErrorCode  DFE_BADNDG           = #{const DFE_BADNDG        }
toHDFErrorCode  DFE_VGSIZE           = #{const DFE_VGSIZE        }
toHDFErrorCode  DFE_VTAB             = #{const DFE_VTAB          }
toHDFErrorCode  DFE_CANTADDELEM      = #{const DFE_CANTADDELEM   }
toHDFErrorCode  DFE_BADVGNAME        = #{const DFE_BADVGNAME     }
toHDFErrorCode  DFE_BADVGCLASS       = #{const DFE_BADVGCLASS    }
toHDFErrorCode  DFE_BADFIELDS        = #{const DFE_BADFIELDS     }
toHDFErrorCode  DFE_NOVS             = #{const DFE_NOVS          }
toHDFErrorCode  DFE_SYMSIZE          = #{const DFE_SYMSIZE       }
toHDFErrorCode  DFE_BADATTACH        = #{const DFE_BADATTACH     }
toHDFErrorCode  DFE_BADVSNAME        = #{const DFE_BADVSNAME     }
toHDFErrorCode  DFE_BADVSCLASS       = #{const DFE_BADVSCLASS    }
toHDFErrorCode  DFE_VSWRITE          = #{const DFE_VSWRITE       }
toHDFErrorCode  DFE_VSREAD           = #{const DFE_VSREAD        }
toHDFErrorCode  DFE_BADVH            = #{const DFE_BADVH         }
toHDFErrorCode  DFE_FIELDSSET        = #{const DFE_FIELDSSET     }
toHDFErrorCode  DFE_VSCANTCREATE     = #{const DFE_VSCANTCREATE  }
toHDFErrorCode  DFE_VGCANTCREATE     = #{const DFE_VGCANTCREATE  }
toHDFErrorCode  DFE_CANTATTACH       = #{const DFE_CANTATTACH    }
toHDFErrorCode  DFE_CANTDETACH       = #{const DFE_CANTDETACH    }
toHDFErrorCode  DFE_XDRERROR         = #{const DFE_XDRERROR      }
toHDFErrorCode  DFE_BITREAD          = #{const DFE_BITREAD       }
toHDFErrorCode  DFE_BITWRITE         = #{const DFE_BITWRITE      }
toHDFErrorCode  DFE_BITSEEK          = #{const DFE_BITSEEK       }
toHDFErrorCode  DFE_TBBTINS          = #{const DFE_TBBTINS       }
toHDFErrorCode  DFE_BVNEW            = #{const DFE_BVNEW         }
toHDFErrorCode  DFE_BVSET            = #{const DFE_BVSET         }
toHDFErrorCode  DFE_BVGET            = #{const DFE_BVGET         }
toHDFErrorCode  DFE_BVFIND           = #{const DFE_BVFIND        }
toHDFErrorCode  DFE_CANTSETATTR      = #{const DFE_CANTSETATTR   }
toHDFErrorCode  DFE_CANTGETATTR      = #{const DFE_CANTGETATTR   }
toHDFErrorCode  DFE_ANAPIERROR       = #{const DFE_ANAPIERROR    }
toHDFErrorCode (DFE_UNKNOWN_ERROR e) = fromIntegral e
