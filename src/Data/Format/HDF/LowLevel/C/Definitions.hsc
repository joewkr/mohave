{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Format.HDF.LowLevel.C.Definitions where

import           Data.Int
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable

#include <hdf.h>
#include <mfhdf.h>

newtype HDFOpenOption = HDFOpenOption { unHDFOpenOption :: Int32 }

#{enum HDFOpenOption, HDFOpenOption
  , hdf_read                 = DFACC_READ
  , hdf_write                = DFACC_WRITE
  , hdf_create               = DFACC_CREATE
  }

newtype HDFDataTypeTag = HDFDataTypeTag { unHDFDataTypeTag :: Int32 }

#{enum HDFDataTypeTag, HDFDataTypeTag
  , hdf_char8                = DFNT_CHAR8
  , hdf_uchar8               = DFNT_UCHAR8
  , hdf_int8                 = DFNT_INT8
  , hdf_uint8                = DFNT_UINT8
  , hdf_int16                = DFNT_INT16
  , hdf_uint16               = DFNT_UINT16
  , hdf_int32                = DFNT_INT32
  , hdf_uint32               = DFNT_UINT32
  , hdf_float32              = DFNT_FLOAT32
  , hdf_float64              = DFNT_FLOAT64
  }

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

tagAlignement, tagSize :: Int
tagAlignement = alignment (undefined :: #{type comp_coder_t})
tagSize = sizeOf (undefined :: #{type comp_coder_t})

tagPtr :: Ptr HDFCompParams -> Ptr #{type comp_coder_t}
tagPtr compParamsPtr = (flip alignPtr) tagAlignement $ plusPtr compParamsPtr #{size comp_info}

instance Storable HDFCompParams where
  alignment _ = #{alignment comp_info}
  sizeOf _ =
    (#{size comp_info} + #{alignment comp_info} + tagAlignement - 1)
    `div` tagAlignement - #{alignment comp_info} + tagSize
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
        return $! HDFCompDeflate skp_size
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
  poke ptr HDFCompNone = embedCompTag ptr #{const COMP_CODE_NONE}
  poke ptr HDFCompRLE = embedCompTag ptr #{const COMP_CODE_RLE}
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
        embedCompTag ptr #{const COMP_CODE_NBIT}
  poke ptr (HDFCompSkHuff skp_size) = do
    #{poke comp_info, skphuff.skp_size} ptr skp_size
    embedCompTag ptr #{const COMP_CODE_SKPHUFF}
  poke ptr (HDFCompDeflate comp_level) = do
    #{poke comp_info, deflate.level} ptr comp_level
    embedCompTag ptr #{const COMP_CODE_DEFLATE}
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
        embedCompTag ptr #{const COMP_CODE_SZIP}

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

embedCompTag :: Ptr HDFCompParams -> #{type comp_coder_t} -> IO ()
embedCompTag compParamsPtr tag =
  poke (tagPtr compParamsPtr) tag
