module Data.Format.HDF.LowLevel.Definitions where

import           Data.Int
import           Data.Word
import           Foreign.Storable (Storable)

data HDFData

class Storable a => HDFDataType a

instance HDFDataType Word8

instance HDFDataType Word16

instance HDFDataType Word32

instance HDFDataType Int8

instance HDFDataType Int16

instance HDFDataType Int32

instance HDFDataType Float

instance HDFDataType Double
