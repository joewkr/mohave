{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Format.NetCDF.LowLevel.Error(
    NCError(..)
  , nc_strerror
  , fromNCErrorCode
  , toNCErrorCode) where

import           Control.Exception (Exception, displayException)
import           Foreign.C.String
import           Foreign.C.Types
import           System.IO.Unsafe (unsafePerformIO)

#include <netcdf.h>

data NCError =
    NC_NOERR
  | NC2_ERR

  | NC_EBADID
  | NC_ENFILE
  | NC_EEXIST
  | NC_EINVAL
  | NC_EPERM

  | NC_ENOTINDEFINE

  | NC_EINDEFINE

  | NC_EINVALCOORDS

  | NC_EMAXDIMS

  | NC_ENAMEINUSE
  | NC_ENOTATT
  | NC_EMAXATTS
  | NC_EBADTYPE
  | NC_EBADDIM
  | NC_EUNLIMPOS

  | NC_EMAXVARS

  | NC_ENOTVAR
  | NC_EGLOBAL
  | NC_ENOTNC
  | NC_ESTS
  | NC_EMAXNAME
  | NC_EUNLIMIT
  | NC_ENORECVARS
  | NC_ECHAR

  | NC_EEDGE
  | NC_ESTRIDE
  | NC_EBADNAME

  | NC_ERANGE
  | NC_ENOMEM
  | NC_EVARSIZE
  | NC_EDIMSIZE
  | NC_ETRUNC
  | NC_EAXISTYPE

  | NC_EDAP
  | NC_ECURL
  | NC_EIO
  | NC_ENODATA
  | NC_EDAPSVC
  | NC_EDAS
  | NC_EDDS
--  | NC_EDMR     -- #define NC_EDMR         NC_EDDS        /**< Dap4 alias */
  | NC_EDATADDS
--  | NC_EDATADAP -- #define NC_EDATADAP     NC_EDATADDS    /**< Dap4 alias */
  | NC_EDAPURL
  | NC_EDAPCONSTRAINT
  | NC_ETRANSLATION
  | NC_EACCESS
  | NC_EAUTH

  | NC_ENOTFOUND
  | NC_ECANTREMOVE
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,1)
  | NC_EINTERNAL
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
  | NC_EPNETCDF
#endif

--  | NC4_FIRST_ERROR
  | NC_EHDFERR
  | NC_ECANTREAD
  | NC_ECANTWRITE
  | NC_ECANTCREATE
  | NC_EFILEMETA
  | NC_EDIMMETA
  | NC_EATTMETA
  | NC_EVARMETA
  | NC_ENOCOMPOUND
  | NC_EATTEXISTS
  | NC_ENOTNC4
  | NC_ESTRICTNC3
  | NC_ENOTNC3
  | NC_ENOPAR
  | NC_EPARINIT
  | NC_EBADGRPID
  | NC_EBADTYPID
  | NC_ETYPDEFINED
  | NC_EBADFIELD
  | NC_EBADCLASS
  | NC_EMAPTYPE
  | NC_ELATEFILL
  | NC_ELATEDEF
  | NC_EDIMSCALE
  | NC_ENOGRP
  | NC_ESTORAGE
  | NC_EBADCHUNK
  | NC_ENOTBUILT
  | NC_EDISKLESS
  | NC_ECANTEXTEND
  | NC_EMPI

  | NC_EFILTER
  | NC_ERCFILE
  | NC_ENULLPAD
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
  | NC_EINMEMORY
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,7,4)
  | NC_ENOFILTER
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,8,0)
  | NC_ENCZARR
  | NC_ES3
  | NC_EEMPTY
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,8,1)
  | NC_EOBJECT
  | NC_ENOOBJECT
  | NC_EPLUGIN
#endif
--  | NC4_LAST_ERROR
  | NC_UNEXPECTED
  | NC_OTHER_ERROR CInt
  deriving (Show, Eq)

instance Exception NCError where
  displayException e = show e ++ ":\t" ++ unsafePerformIO (nc_strerror e)

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
  #{const NC4_LAST_ERROR - 1} -> NC_UNEXPECTED
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
toNCErrorCode NC_UNEXPECTED      = #{const NC4_LAST_ERROR - 1}
toNCErrorCode (NC_OTHER_ERROR e) = e

foreign import ccall unsafe "nc_strerror" c_nc_strerror :: CInt -> IO CString

nc_strerror :: NCError -> IO String
nc_strerror NC_UNEXPECTED = return "mohave NetCDF: unexpected value was returned after a foreign call"
nc_strerror ncerr = c_nc_strerror (toNCErrorCode ncerr) >>= peekCString
