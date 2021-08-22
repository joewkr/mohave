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

fromNCErrorCode :: CInt -> NCError
fromNCErrorCode e = case e of
  #{const NC_NOERR          } -> NC_NOERR
  #{const NC2_ERR           } -> NC2_ERR

  #{const NC_EBADID         } -> NC_EBADID
  #{const NC_ENFILE         } -> NC_ENFILE
  #{const NC_EEXIST         } -> NC_EEXIST
  #{const NC_EINVAL         } -> NC_EINVAL
  #{const NC_EPERM          } -> NC_EPERM

  #{const NC_ENOTINDEFINE   } -> NC_ENOTINDEFINE

  #{const NC_EINDEFINE      } -> NC_EINDEFINE

  #{const NC_EINVALCOORDS   } -> NC_EINVALCOORDS

  #{const NC_EMAXDIMS       } -> NC_EMAXDIMS

  #{const NC_ENAMEINUSE     } -> NC_ENAMEINUSE
  #{const NC_ENOTATT        } -> NC_ENOTATT
  #{const NC_EMAXATTS       } -> NC_EMAXATTS
  #{const NC_EBADTYPE       } -> NC_EBADTYPE
  #{const NC_EBADDIM        } -> NC_EBADDIM
  #{const NC_EUNLIMPOS      } -> NC_EUNLIMPOS

  #{const NC_EMAXVARS       } -> NC_EMAXVARS

  #{const NC_ENOTVAR        } -> NC_ENOTVAR
  #{const NC_EGLOBAL        } -> NC_EGLOBAL
  #{const NC_ENOTNC         } -> NC_ENOTNC
  #{const NC_ESTS           } -> NC_ESTS
  #{const NC_EMAXNAME       } -> NC_EMAXNAME
  #{const NC_EUNLIMIT       } -> NC_EUNLIMIT
  #{const NC_ENORECVARS     } -> NC_ENORECVARS
  #{const NC_ECHAR          } -> NC_ECHAR

  #{const NC_EEDGE          } -> NC_EEDGE
  #{const NC_ESTRIDE        } -> NC_ESTRIDE
  #{const NC_EBADNAME       } -> NC_EBADNAME

  #{const NC_ERANGE         } -> NC_ERANGE
  #{const NC_ENOMEM         } -> NC_ENOMEM
  #{const NC_EVARSIZE       } -> NC_EVARSIZE
  #{const NC_EDIMSIZE       } -> NC_EDIMSIZE
  #{const NC_ETRUNC         } -> NC_ETRUNC
  #{const NC_EAXISTYPE      } -> NC_EAXISTYPE

  #{const NC_EDAP           } -> NC_EDAP
  #{const NC_ECURL          } -> NC_ECURL
  #{const NC_EIO            } -> NC_EIO
  #{const NC_ENODATA        } -> NC_ENODATA
  #{const NC_EDAPSVC        } -> NC_EDAPSVC
  #{const NC_EDAS           } -> NC_EDAS
  #{const NC_EDDS           } -> NC_EDDS
-- #{const NC_EDMR          } -> NC_EDMR
  #{const NC_EDATADDS       } -> NC_EDATADDS
-- #{const NC_EDATADAP      } -> NC_EDATADAP
  #{const NC_EDAPURL        } -> NC_EDAPURL
  #{const NC_EDAPCONSTRAINT } -> NC_EDAPCONSTRAINT
  #{const NC_ETRANSLATION   } -> NC_ETRANSLATION
  #{const NC_EACCESS        } -> NC_EACCESS
  #{const NC_EAUTH          } -> NC_EAUTH

  #{const NC_ENOTFOUND      } -> NC_ENOTFOUND
  #{const NC_ECANTREMOVE    } -> NC_ECANTREMOVE
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,1)
  #{const NC_EINTERNAL      } -> NC_EINTERNAL
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
  #{const NC_EPNETCDF       } -> NC_EPNETCDF
#endif

-- #{const NC4_FIRST_ERROR  } -> NC4_FIRST_ERROR
  #{const NC_EHDFERR        } -> NC_EHDFERR
  #{const NC_ECANTREAD      } -> NC_ECANTREAD
  #{const NC_ECANTWRITE     } -> NC_ECANTWRITE
  #{const NC_ECANTCREATE    } -> NC_ECANTCREATE
  #{const NC_EFILEMETA      } -> NC_EFILEMETA
  #{const NC_EDIMMETA       } -> NC_EDIMMETA
  #{const NC_EATTMETA       } -> NC_EATTMETA
  #{const NC_EVARMETA       } -> NC_EVARMETA
  #{const NC_ENOCOMPOUND    } -> NC_ENOCOMPOUND
  #{const NC_EATTEXISTS     } -> NC_EATTEXISTS
  #{const NC_ENOTNC4        } -> NC_ENOTNC4
  #{const NC_ESTRICTNC3     } -> NC_ESTRICTNC3
  #{const NC_ENOTNC3        } -> NC_ENOTNC3
  #{const NC_ENOPAR         } -> NC_ENOPAR
  #{const NC_EPARINIT       } -> NC_EPARINIT
  #{const NC_EBADGRPID      } -> NC_EBADGRPID
  #{const NC_EBADTYPID      } -> NC_EBADTYPID
  #{const NC_ETYPDEFINED    } -> NC_ETYPDEFINED
  #{const NC_EBADFIELD      } -> NC_EBADFIELD
  #{const NC_EBADCLASS      } -> NC_EBADCLASS
  #{const NC_EMAPTYPE       } -> NC_EMAPTYPE
  #{const NC_ELATEFILL      } -> NC_ELATEFILL
  #{const NC_ELATEDEF       } -> NC_ELATEDEF
  #{const NC_EDIMSCALE      } -> NC_EDIMSCALE
  #{const NC_ENOGRP         } -> NC_ENOGRP
  #{const NC_ESTORAGE       } -> NC_ESTORAGE
  #{const NC_EBADCHUNK      } -> NC_EBADCHUNK
  #{const NC_ENOTBUILT      } -> NC_ENOTBUILT
  #{const NC_EDISKLESS      } -> NC_EDISKLESS
  #{const NC_ECANTEXTEND    } -> NC_ECANTEXTEND
  #{const NC_EMPI           } -> NC_EMPI

  #{const NC_EFILTER        } -> NC_EFILTER
  #{const NC_ERCFILE        } -> NC_ERCFILE
  #{const NC_ENULLPAD       } -> NC_ENULLPAD
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
  #{const NC_EINMEMORY      } -> NC_EINMEMORY
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,7,4)
  #{const NC_ENOFILTER      } -> NC_ENOFILTER
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,8,0)
  #{const NC_ENCZARR        } -> NC_ENCZARR
  #{const NC_ES3            } -> NC_ES3
  #{const NC_EEMPTY         } -> NC_EEMPTY
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,8,1)
  #{const NC_EOBJECT        } -> NC_EOBJECT
  #{const NC_ENOOBJECT      } -> NC_ENOOBJECT
  #{const NC_EPLUGIN        } -> NC_EPLUGIN
#endif

-- #{const NC4_LAST_ERROR   } -> NC4_LAST_ERROR
  _                           -> NC_OTHER_ERROR e

toNCErrorCode :: NCError -> CInt
toNCErrorCode NC_NOERR           = #{const NC_NOERR          }
toNCErrorCode NC2_ERR            = #{const NC2_ERR           }

toNCErrorCode NC_EBADID          = #{const NC_EBADID         }
toNCErrorCode NC_ENFILE          = #{const NC_ENFILE         }
toNCErrorCode NC_EEXIST          = #{const NC_EEXIST         }
toNCErrorCode NC_EINVAL          = #{const NC_EINVAL         }
toNCErrorCode NC_EPERM           = #{const NC_EPERM          }

toNCErrorCode NC_ENOTINDEFINE    = #{const NC_ENOTINDEFINE   }

toNCErrorCode NC_EINDEFINE       = #{const NC_EINDEFINE      }

toNCErrorCode NC_EINVALCOORDS    = #{const NC_EINVALCOORDS   }

toNCErrorCode NC_EMAXDIMS        = #{const NC_EMAXDIMS       }

toNCErrorCode NC_ENAMEINUSE      = #{const NC_ENAMEINUSE     }
toNCErrorCode NC_ENOTATT         = #{const NC_ENOTATT        }
toNCErrorCode NC_EMAXATTS        = #{const NC_EMAXATTS       }
toNCErrorCode NC_EBADTYPE        = #{const NC_EBADTYPE       }
toNCErrorCode NC_EBADDIM         = #{const NC_EBADDIM        }
toNCErrorCode NC_EUNLIMPOS       = #{const NC_EUNLIMPOS      }

toNCErrorCode NC_EMAXVARS        = #{const NC_EMAXVARS       }

toNCErrorCode NC_ENOTVAR         = #{const NC_ENOTVAR        }
toNCErrorCode NC_EGLOBAL         = #{const NC_EGLOBAL        }
toNCErrorCode NC_ENOTNC          = #{const NC_ENOTNC         }
toNCErrorCode NC_ESTS            = #{const NC_ESTS           }
toNCErrorCode NC_EMAXNAME        = #{const NC_EMAXNAME       }
toNCErrorCode NC_EUNLIMIT        = #{const NC_EUNLIMIT       }
toNCErrorCode NC_ENORECVARS      = #{const NC_ENORECVARS     }
toNCErrorCode NC_ECHAR           = #{const NC_ECHAR          }

toNCErrorCode NC_EEDGE           = #{const NC_EEDGE          }
toNCErrorCode NC_ESTRIDE         = #{const NC_ESTRIDE        }
toNCErrorCode NC_EBADNAME        = #{const NC_EBADNAME       }

toNCErrorCode NC_ERANGE          = #{const NC_ERANGE         }
toNCErrorCode NC_ENOMEM          = #{const NC_ENOMEM         }
toNCErrorCode NC_EVARSIZE        = #{const NC_EVARSIZE       }
toNCErrorCode NC_EDIMSIZE        = #{const NC_EDIMSIZE       }
toNCErrorCode NC_ETRUNC          = #{const NC_ETRUNC         }
toNCErrorCode NC_EAXISTYPE       = #{const NC_EAXISTYPE      }

toNCErrorCode NC_EDAP            = #{const NC_EDAP           }
toNCErrorCode NC_ECURL           = #{const NC_ECURL          }
toNCErrorCode NC_EIO             = #{const NC_EIO            }
toNCErrorCode NC_ENODATA         = #{const NC_ENODATA        }
toNCErrorCode NC_EDAPSVC         = #{const NC_EDAPSVC        }
toNCErrorCode NC_EDAS            = #{const NC_EDAS           }
toNCErrorCode NC_EDDS            = #{const NC_EDDS           }
-- toNCErrorCode NC_EDMR         = #{const NC_EDMR           }
toNCErrorCode NC_EDATADDS        = #{const NC_EDATADDS       }
-- toNCErrorCode NC_EDATADAP     = #{const NC_EDATADAP       }
toNCErrorCode NC_EDAPURL         = #{const NC_EDAPURL        }
toNCErrorCode NC_EDAPCONSTRAINT  = #{const NC_EDAPCONSTRAINT }
toNCErrorCode NC_ETRANSLATION    = #{const NC_ETRANSLATION   }
toNCErrorCode NC_EACCESS         = #{const NC_EACCESS        }
toNCErrorCode NC_EAUTH           = #{const NC_EAUTH          }

toNCErrorCode NC_ENOTFOUND       = #{const NC_ENOTFOUND      }
toNCErrorCode NC_ECANTREMOVE     = #{const NC_ECANTREMOVE    }
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,1)
toNCErrorCode NC_EINTERNAL       = #{const NC_EINTERNAL      }
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
toNCErrorCode NC_EPNETCDF        = #{const NC_EPNETCDF       }
#endif

--toNCErrorCode NC4_FIRST_ERROR  = #{const NC4_FIRST_ERROR   }
toNCErrorCode NC_EHDFERR         = #{const NC_EHDFERR        }
toNCErrorCode NC_ECANTREAD       = #{const NC_ECANTREAD      }
toNCErrorCode NC_ECANTWRITE      = #{const NC_ECANTWRITE     }
toNCErrorCode NC_ECANTCREATE     = #{const NC_ECANTCREATE    }
toNCErrorCode NC_EFILEMETA       = #{const NC_EFILEMETA      }
toNCErrorCode NC_EDIMMETA        = #{const NC_EDIMMETA       }
toNCErrorCode NC_EATTMETA        = #{const NC_EATTMETA       }
toNCErrorCode NC_EVARMETA        = #{const NC_EVARMETA       }
toNCErrorCode NC_ENOCOMPOUND     = #{const NC_ENOCOMPOUND    }
toNCErrorCode NC_EATTEXISTS      = #{const NC_EATTEXISTS     }
toNCErrorCode NC_ENOTNC4         = #{const NC_ENOTNC4        }
toNCErrorCode NC_ESTRICTNC3      = #{const NC_ESTRICTNC3     }
toNCErrorCode NC_ENOTNC3         = #{const NC_ENOTNC3        }
toNCErrorCode NC_ENOPAR          = #{const NC_ENOPAR         }
toNCErrorCode NC_EPARINIT        = #{const NC_EPARINIT       }
toNCErrorCode NC_EBADGRPID       = #{const NC_EBADGRPID      }
toNCErrorCode NC_EBADTYPID       = #{const NC_EBADTYPID      }
toNCErrorCode NC_ETYPDEFINED     = #{const NC_ETYPDEFINED    }
toNCErrorCode NC_EBADFIELD       = #{const NC_EBADFIELD      }
toNCErrorCode NC_EBADCLASS       = #{const NC_EBADCLASS      }
toNCErrorCode NC_EMAPTYPE        = #{const NC_EMAPTYPE       }
toNCErrorCode NC_ELATEFILL       = #{const NC_ELATEFILL      }
toNCErrorCode NC_ELATEDEF        = #{const NC_ELATEDEF       }
toNCErrorCode NC_EDIMSCALE       = #{const NC_EDIMSCALE      }
toNCErrorCode NC_ENOGRP          = #{const NC_ENOGRP         }
toNCErrorCode NC_ESTORAGE        = #{const NC_ESTORAGE       }
toNCErrorCode NC_EBADCHUNK       = #{const NC_EBADCHUNK      }
toNCErrorCode NC_ENOTBUILT       = #{const NC_ENOTBUILT      }
toNCErrorCode NC_EDISKLESS       = #{const NC_EDISKLESS      }
toNCErrorCode NC_ECANTEXTEND     = #{const NC_ECANTEXTEND    }
toNCErrorCode NC_EMPI            = #{const NC_EMPI           }

toNCErrorCode NC_EFILTER         = #{const NC_EFILTER        }
toNCErrorCode NC_ERCFILE         = #{const NC_ERCFILE        }
toNCErrorCode NC_ENULLPAD        = #{const NC_ENULLPAD       }
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
toNCErrorCode NC_EINMEMORY       = #{const NC_EINMEMORY      }
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,7,4)
toNCErrorCode NC_ENOFILTER       = #{const NC_ENOFILTER      }
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,8,0)
toNCErrorCode NC_ENCZARR         = #{const NC_ENCZARR        }
toNCErrorCode NC_ES3             = #{const NC_ES3            }
toNCErrorCode NC_EEMPTY          = #{const NC_EEMPTY         }
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,8,1)
toNCErrorCode NC_EOBJECT         = #{const NC_EOBJECT        }
toNCErrorCode NC_ENOOBJECT       = #{const NC_ENOOBJECT      }
toNCErrorCode NC_EPLUGIN         = #{const NC_EPLUGIN        }
#endif

--toNCErrorCode NC4_LAST_ERROR   = #{const NC4_LAST_ERROR    }
toNCErrorCode (NC_OTHER_ERROR e) = e
