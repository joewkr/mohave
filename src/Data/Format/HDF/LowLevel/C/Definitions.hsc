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

newtype HDFOpenOption = HDFOpenOption { unHDFOpenOption :: Int32 }

#{enum HDFOpenOption, HDFOpenOption
  , hdf_read                 = DFACC_READ
  , hdf_write                = DFACC_WRITE
  , hdf_create               = DFACC_CREATE
  }

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

    #{const (DFNT_HDF    | DFNT_CHAR8  )} -> HDFValue HInt8    ()
    #{const (DFNT_HDF    | DFNT_UCHAR8 )} -> HDFValue HWord8   ()

    #{const (DFNT_NATIVE | DFNT_UINT8  )} -> HDFValue HWord8   ()
    #{const (DFNT_NATIVE | DFNT_UINT16 )} -> HDFValue HWord16  ()
    #{const (DFNT_NATIVE | DFNT_UINT32 )} -> HDFValue HWord32  ()
    #{const (DFNT_NATIVE | DFNT_INT8   )} -> HDFValue HInt8    ()
    #{const (DFNT_NATIVE | DFNT_INT16  )} -> HDFValue HInt16   ()
    #{const (DFNT_NATIVE | DFNT_INT32  )} -> HDFValue HInt32   ()
    #{const (DFNT_NATIVE | DFNT_FLOAT32)} -> HDFValue HFloat32 ()
    #{const (DFNT_NATIVE | DFNT_FLOAT64)} -> HDFValue HFloat64 ()

    #{const (DFNT_NATIVE | DFNT_CHAR8  )} -> HDFValue HInt8    ()
    #{const (DFNT_NATIVE | DFNT_UCHAR8 )} -> HDFValue HWord8   ()

    #{const (DFNT_CUSTOM | DFNT_UINT8  )} -> HDFValue HWord8   ()
    #{const (DFNT_CUSTOM | DFNT_UINT16 )} -> HDFValue HWord16  ()
    #{const (DFNT_CUSTOM | DFNT_UINT32 )} -> HDFValue HWord32  ()
    #{const (DFNT_CUSTOM | DFNT_INT8   )} -> HDFValue HInt8    ()
    #{const (DFNT_CUSTOM | DFNT_INT16  )} -> HDFValue HInt16   ()
    #{const (DFNT_CUSTOM | DFNT_INT32  )} -> HDFValue HInt32   ()
    #{const (DFNT_CUSTOM | DFNT_FLOAT32)} -> HDFValue HFloat32 ()
    #{const (DFNT_CUSTOM | DFNT_FLOAT64)} -> HDFValue HFloat64 ()

    #{const (DFNT_CUSTOM | DFNT_CHAR8  )} -> HDFValue HInt8    ()
    #{const (DFNT_CUSTOM | DFNT_UCHAR8 )} -> HDFValue HWord8   ()

    #{const (DFNT_LITEND | DFNT_UINT8  )} -> HDFValue HWord8   ()
    #{const (DFNT_LITEND | DFNT_UINT16 )} -> HDFValue HWord16  ()
    #{const (DFNT_LITEND | DFNT_UINT32 )} -> HDFValue HWord32  ()
    #{const (DFNT_LITEND | DFNT_INT8   )} -> HDFValue HInt8    ()
    #{const (DFNT_LITEND | DFNT_INT16  )} -> HDFValue HInt16   ()
    #{const (DFNT_LITEND | DFNT_INT32  )} -> HDFValue HInt32   ()
    #{const (DFNT_LITEND | DFNT_FLOAT32)} -> HDFValue HFloat32 ()
    #{const (DFNT_LITEND | DFNT_FLOAT64)} -> HDFValue HFloat64 ()

    #{const (DFNT_LITEND | DFNT_CHAR8  )} -> HDFValue HInt8    ()
    #{const (DFNT_LITEND | DFNT_UCHAR8 )} -> HDFValue HWord8   ()

    _                     -> HDFValue HNone    ()

toHDFTypeTag :: HDFType -> Int32
toHDFTypeTag (HDFValue t _) = fromHDataType t

fromHDataType :: HDataType a -> Int32
fromHDataType HNone    = 0
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

newtype HDFFillModeTag = HDFFillModeTag { unHDFFillModeTag :: Int32 }

#{enum HDFFillModeTag, HDFFillModeTag
  , hdf_fill                 = SD_FILL
  , hdf_nofill               = SD_NOFILL
  }

data HDFCompParams =
    HDFCompNone
  | HDFCompRLE
  | HDFCompNBit {
      hdfCompRawType :: Int32
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
          rawType
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
        #{poke comp_info, nbit.nt} ptr rawType
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

newtype HDFCompModeTag = HDFCompModeTag { unHDFCompModeTag :: #{type comp_coder_t} }

type HDFCompType = #{type comp_coder_t}

#{enum HDFCompModeTag, HDFCompModeTag
  , hdf_comp_none            = COMP_CODE_NONE
  , hdf_comp_rle             = COMP_CODE_RLE
  , hdf_comp_nbit            = COMP_CODE_NBIT
  , hdf_comp_skphuff         = COMP_CODE_SKPHUFF
  , hdf_comp_deflate         = COMP_CODE_DEFLATE
  , hdf_comp_szip            = COMP_CODE_SZIP
  }

selectCompMode :: HDFCompParams -> HDFCompModeTag
selectCompMode HDFCompNone{}    = hdf_comp_none
selectCompMode HDFCompRLE{}     = hdf_comp_rle
selectCompMode HDFCompNBit{}    = hdf_comp_nbit
selectCompMode HDFCompSkHuff{}  = hdf_comp_skphuff
selectCompMode HDFCompDeflate{} = hdf_comp_deflate
selectCompMode HDFCompSZip{}    = hdf_comp_szip

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
            0
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
        compTag = (fromIntegral . unHDFCompModeTag $ selectCompMode compParams) :: Int32
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

newtype AnnTypeTag = AnnTypeTag { unAnnTypeTag :: CAnnType }

#{enum AnnTypeTag, AnnTypeTag
  , hdf_ann_undef      = AN_UNDEF
  , hdf_ann_data_label = AN_DATA_LABEL
  , hdf_ann_data_desc  = AN_DATA_DESC
  , hdf_ann_file_label = AN_FILE_LABEL
  , hdf_ann_file_desc  = AN_FILE_DESC
  }
