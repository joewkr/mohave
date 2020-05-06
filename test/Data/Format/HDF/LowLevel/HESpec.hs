module Data.Format.HDF.LowLevel.HESpec(spec) where

import           Data.Format.HDF.LowLevel.HE
import           Data.Format.HDF.LowLevel.SD
import           Data.Int
import           Foreign.C.Types
import           Test.Hspec
import           Testing.Common

spec :: Spec
spec = do
    describe "HDF error reporting" $ do
        it "dumps HDF errors to file" $ do
            let logFileName = "hdf-error-trace.log"
                expectedFirstLine = "HDF error: (5) <Bad file name on open>"
            (status_1, _) <-           sd_start "test-data/sd/NULL" HDFRead
            _             <-           he_print logFileName
            firstLine     <- (head . lines) <$> readFile logFileName
            status_1 `shouldBe` (-1)
            firstLine `shouldBe` expectedFirstLine
        it "produces an empty error dump file when no errors reported" $ do
            let logFileName = "hdf-error-trace.no-errors.log"
            sd_id         <- check =<< sd_start "test-data/sd/test1.hdf" HDFRead
            _             <- check =<< sd_end sd_id
            _             <-           he_print logFileName
            contents      <-           readFile logFileName
            contents `shouldBe` []
        it "reports HDF error code" $ do
            (status_1, _) <-           sd_start "test-data/sd/NULL" HDFRead
            he            <-           he_value 1
            status_1 `shouldBe` (-1)
            he       `shouldBe` DFE_BADNAME
        it "converts HDF error code to string" $ do
            he_string DFE_NONE          `shouldReturn` "No error"
            he_string DFE_FNF           `shouldReturn` "File not found"
            he_string DFE_DENIED        `shouldReturn` "Access to file denied"
            he_string DFE_ALROPEN       `shouldReturn` "File already open"
            he_string DFE_TOOMANY       `shouldReturn` "Too Many AID's or files open"
            he_string DFE_BADNAME       `shouldReturn` "Bad file name on open"
            he_string DFE_BADACC        `shouldReturn` "Bad file access mode"
            he_string DFE_BADOPEN       `shouldReturn` "Error opening file"
            he_string DFE_NOTOPEN       `shouldReturn` "File can't be closed; It isn't open"
            he_string DFE_CANTCLOSE     `shouldReturn` "Unable to close file"
            he_string DFE_READERROR     `shouldReturn` "Read error"
            he_string DFE_WRITEERROR    `shouldReturn` "Write error"
            he_string DFE_SEEKERROR     `shouldReturn` "Error performing seek operation"
            he_string DFE_RDONLY        `shouldReturn` "Attempt to write to read-only HDF file"
            he_string DFE_BADSEEK       `shouldReturn` "Attempt to seek past end of element"
            he_string DFE_INVFILE       `shouldReturn` "File is supported, must be either hdf, cdf, netcdf"
            he_string DFE_PUTELEM       `shouldReturn` "Hputelement failed in some way"
            he_string DFE_GETELEM       `shouldReturn` "Hgetelement failed in some way"
            he_string DFE_CANTLINK      `shouldReturn` "Can't initialize link information"
            he_string DFE_CANTSYNC      `shouldReturn` "Cannot syncronize memory with file"
            he_string DFE_BADGROUP      `shouldReturn` "Error from DFdiread in opening a group"
            he_string DFE_GROUPSETUP    `shouldReturn` "Error from DFdisetup in opening a group"
            he_string DFE_PUTGROUP      `shouldReturn` "Error when putting a tag/ref into a group"
            he_string DFE_GROUPWRITE    `shouldReturn` "Error when writing out a group"
            he_string DFE_DFNULL        `shouldReturn` "DF has a null pointer"
            he_string DFE_ILLTYPE       `shouldReturn` "Internal error: DF has an illegal type"
            he_string DFE_BADDDLIST     `shouldReturn` "Internal error: The DD list is non-existent"
            he_string DFE_NOTDFFILE     `shouldReturn` "This is not an HDF file"
            he_string DFE_SEEDTWICE     `shouldReturn` "Internal error: The DD list is already seeded"
            he_string DFE_NOSUCHTAG     `shouldReturn` "No such tag in the file: search failed"
            he_string DFE_NOFREEDD      `shouldReturn` "There are no free DD's left"
            he_string DFE_BADTAG        `shouldReturn` "Illegal WILDCARD tag"
            he_string DFE_BADREF        `shouldReturn` "Illegal WILDCARD reference"
            he_string DFE_NOMATCH       `shouldReturn` "No (more) DDs which match specified tag/ref"
            he_string DFE_NOTINSET      `shouldReturn` "Set contained unknown tag: ignored"
            he_string DFE_BADOFFSET     `shouldReturn` "Illegal offset specified"
            he_string DFE_CORRUPT       `shouldReturn` "File is corrupted"
            he_string DFE_NOREF         `shouldReturn` "No more reference numbers are available"
            he_string DFE_DUPDD         `shouldReturn` "Tag/ref is already used"
            he_string DFE_CANTMOD       `shouldReturn` "Old element does not exist, cannot modify"
            he_string DFE_DIFFFILES     `shouldReturn` "Attempt to merge objects in different files"
            he_string DFE_BADAID        `shouldReturn` "Unable to create a new AID"
            he_string DFE_OPENAID       `shouldReturn` "There are still active AIDs"
            he_string DFE_CANTFLUSH     `shouldReturn` "Cannot flush the changed DD back to the file"
            he_string DFE_CANTUPDATE    `shouldReturn` "Cannot update the DD block"
            he_string DFE_CANTHASH      `shouldReturn` "Cannot add a DD to the hash table"
            he_string DFE_CANTDELDD     `shouldReturn` "Cannot delete a DD in the file"
            he_string DFE_CANTDELHASH   `shouldReturn` "Cannot delete a DD from the hash table"
            he_string DFE_CANTACCESS    `shouldReturn` "Cannot access specified tag/ref"
            he_string DFE_CANTENDACCESS `shouldReturn` "Cannot end access to data element"
            he_string DFE_TABLEFULL     `shouldReturn` "Access table is full"
            he_string DFE_NOTINTABLE    `shouldReturn` "Cannot find element in table"
            he_string DFE_UNSUPPORTED   `shouldReturn` "Feature not currently supported"
            he_string DFE_NOSPACE       `shouldReturn` "Internal error: Out of space"
            he_string DFE_BADCALL       `shouldReturn` "Calls in wrong order"
            he_string DFE_BADPTR        `shouldReturn` "NULL ptr argument"
            he_string DFE_BADLEN        `shouldReturn` "Invalid length specified"
            he_string DFE_NOTENOUGH     `shouldReturn` "Space provided insufficient for size of data"
            he_string DFE_NOVALS        `shouldReturn` "Values not available"
            he_string DFE_ARGS          `shouldReturn` "Invalid arguments to routine"
            he_string DFE_INTERNAL      `shouldReturn` "HDF Internal error"
            he_string DFE_NORESET       `shouldReturn` "Can not reset this value"
            he_string DFE_EXCEEDMAX     `shouldReturn` "Value exceeds max allowed"
            he_string DFE_GENAPP        `shouldReturn` "Generic application-level error"
            he_string DFE_UNINIT        `shouldReturn` "Interface was not initialized correctly"
            he_string DFE_CANTINIT      `shouldReturn` "Can't initialize an interface we depend on"
            he_string DFE_CANTSHUTDOWN  `shouldReturn` "Can't shut down an interface we depend on"
            he_string DFE_BADDIM        `shouldReturn` "Negative or zero dimensions specified"
            he_string DFE_BADFP         `shouldReturn` "File contained an illegal floating point number"
            he_string DFE_BADDATATYPE   `shouldReturn` "Unknown or unavailable data type specified"
            he_string DFE_BADMCTYPE     `shouldReturn` "Unknown or unavailable machine type specified"
            he_string DFE_BADNUMTYPE    `shouldReturn` "Unknown or unavailable number type specified"
            he_string DFE_BADORDER      `shouldReturn` "Unknown or illegal array order specified"
            he_string DFE_RANGE         `shouldReturn` "Improper range for attempted access"
            he_string DFE_BADCONV       `shouldReturn` "Don't know how to convert data type"
            he_string DFE_BADTYPE       `shouldReturn` "Incompatible type specified"
            he_string DFE_BADDIMNAME    `shouldReturn` "Dimension name not valid or already taken"
            he_string DFE_NOVGREP       `shouldReturn` "No Vgroup representation for SDS and dim"
            he_string DFE_BADSCHEME     `shouldReturn` "Unknown compression scheme specified"
            he_string DFE_BADMODEL      `shouldReturn` "Invalid compression model specified"
            he_string DFE_BADCODER      `shouldReturn` "Invalid compression coder specified"
            he_string DFE_MODEL         `shouldReturn` "Error in modeling layer of compression"
            he_string DFE_CODER         `shouldReturn` "Error in encoding layer of compression"
            he_string DFE_CINIT         `shouldReturn` "Error in encoding initialization"
            he_string DFE_CDECODE       `shouldReturn` "Error in decoding compressed data"
            he_string DFE_CENCODE       `shouldReturn` "Error in encoding compressed data"
            he_string DFE_CTERM         `shouldReturn` "Error in encoding termination"
            he_string DFE_CSEEK         `shouldReturn` "Error seeking in encoded dataset"
            he_string DFE_MINIT         `shouldReturn` "Error in modeling initialization"
            he_string DFE_COMPINFO      `shouldReturn` "Invalid compression header"
            he_string DFE_CANTCOMP      `shouldReturn` "Can't compress an object"
            he_string DFE_CANTDECOMP    `shouldReturn` "Can't de-compress an object"
            he_string DFE_NOENCODER     `shouldReturn` "Encoder not available"
            he_string DFE_NOSZLIB       `shouldReturn` "SZIP library not available"
            he_string DFE_COMPVERSION   `shouldReturn` "Z_VERSION_ERROR (-6) returned from zlib"
            he_string DFE_READCOMP      `shouldReturn` "Error in reading compressed data"
            he_string DFE_NODIM         `shouldReturn` "No dimension record associated with image or data set"
            he_string DFE_BADRIG        `shouldReturn` "Error processing a RIG"
            he_string DFE_RINOTFOUND    `shouldReturn` "Can't find raster image"
            he_string DFE_BADATTR       `shouldReturn` "Bad Attribute"
            he_string DFE_LUTNOTFOUND   `shouldReturn` "No palette information for RIG"
            he_string DFE_GRNOTFOUND    `shouldReturn` "Can't find specified GR"
            he_string DFE_BADTABLE      `shouldReturn` "The nsdg table is wrong"
            he_string DFE_BADSDG        `shouldReturn` "Error processing an sdg"
            he_string DFE_BADNDG        `shouldReturn` "Error processing an ndg"
            he_string DFE_VGSIZE        `shouldReturn` "No more elements will fit in this VGroup"
            he_string DFE_VTAB          `shouldReturn` "Element is not in VSet tables"
            he_string DFE_CANTADDELEM   `shouldReturn` "Cannot add tag/ref to VGroup"
            he_string DFE_BADVGNAME     `shouldReturn` "Cannot set VGroup name"
            he_string DFE_BADVGCLASS    `shouldReturn` "Cannot set VGroup class"
            he_string DFE_BADFIELDS     `shouldReturn` "Unable to parse fields string correctly"
            he_string DFE_NOVS          `shouldReturn` "Could not find specified VS or VG in file"
            he_string DFE_SYMSIZE       `shouldReturn` "Too many symbols in table"
            he_string DFE_BADATTACH     `shouldReturn` "Cannot write to a previously attached VData"
            he_string DFE_BADVSNAME     `shouldReturn` "Cannot set VData name"
            he_string DFE_BADVSCLASS    `shouldReturn` "Cannot set VData class"
            he_string DFE_VSWRITE       `shouldReturn` "Error writing to VData"
            he_string DFE_VSREAD        `shouldReturn` "Error reading from VData"
            he_string DFE_BADVH         `shouldReturn` "Error in VData Header"
            he_string DFE_FIELDSSET     `shouldReturn` "Fields already set for vdata"
            he_string DFE_VSCANTCREATE  `shouldReturn` "Cannot create VData"
            he_string DFE_VGCANTCREATE  `shouldReturn` "Cannot create VGroup"
            he_string DFE_CANTATTACH    `shouldReturn` "Cannot attach to a VData"
            he_string DFE_CANTDETACH    `shouldReturn` "Cannot detach a VData with access 'w'"
            he_string DFE_XDRERROR      `shouldReturn` "Error from XDR and/or CDF level"
            he_string DFE_BITREAD       `shouldReturn` "There was a bit-read error"
            he_string DFE_BITWRITE      `shouldReturn` "There was a bit-write error"
            he_string DFE_BITSEEK       `shouldReturn` "There was a bit-seek error"
            he_string DFE_TBBTINS       `shouldReturn` "Failed to insert element into tree"
            he_string DFE_BVNEW         `shouldReturn` "Failed to create a bit-vector"
            he_string DFE_BVSET         `shouldReturn` "Failed when setting a bit in a bit-vector"
            he_string DFE_BVGET         `shouldReturn` "Failed when getting a bit in a bit-vector"
            he_string DFE_BVFIND        `shouldReturn` "Failed when finding a bit in a bit-vector"
            he_string DFE_CANTSETATTR   `shouldReturn` "Cannot set an attribute"
            he_string DFE_CANTGETATTR   `shouldReturn` "Cannot find or get an attribute"
            he_string DFE_ANAPIERROR    `shouldReturn` "Failed in annotation interface"

            he_string (DFE_UNKNOWN_ERROR 999) `shouldReturn` "Unknown error"
        it "handles adding custom message to the error" $ do
            let logFileName = "hdf-error-trace-custom.log"
                expectedFirstLine = "\tCustom HDF error 177"
            (status_1, _) <-           sd_start "test-data/sd/NULL" HDFRead
            _             <-           he_report "Custom HDF error %i" (177 :: Int32)
            _             <-           he_print logFileName
            hdfErrors     <- (take 3 . lines) <$> readFile logFileName
            status_1 `shouldBe` (-1)
            (last hdfErrors) `shouldBe` expectedFirstLine
        it "clears HDF error stack" $ do
            let logFileName = "hdf-error-trace-cleared.log"
            (status_1, _) <-           sd_start "test-data/sd/NULL" HDFRead
            _             <-           he_clear
            _             <-           he_print logFileName
            contents      <-           readFile logFileName
            status_1 `shouldBe` (-1)
            contents `shouldBe` []
        it "builds custom HDF error stack" $ do
            let logFileName = "hdf-error-trace-custom-2.log"
                expectedTrace = "HDF error: (2) <Access to file denied>\n\
                                \\tDetected in spec() [HESpec.hs line 1122]\n\
                                \\tCustom HDF error report 1.23\n"
            _             <-           he_clear
            _             <-           he_push DFE_DENIED "spec" "HESpec.hs" 1122
            _             <-           he_report "Custom HDF error report %4.2f" (1.23 :: CDouble)
            _             <-           he_print logFileName
            contents      <-           readFile logFileName
            contents `shouldBe` expectedTrace
        {-
        -- Non-deterministic test, expects that if memory storing the file name has been
        -- freed too early it could be overwritten and result in corrupted output. When this
        -- problem was not fixed yet it could take a few thousand iterations to generate
        -- corrupted output.
        --
        -- Requires:
        -- import           Control.Monad (forM_)
        -- import           Foreign.C.String (newCString)
        -- import           System.Mem (performGC)

        it "keeps correct HDF error stack after GC, 10000 iterations" $ do
            let logFileName = "/dev/shm/hdf-error-trace-custom-3.log"
            forM_ [1..10000] $ \index -> do
                let expectedTrace = "HDF error: (2) <Access to file denied>\n\
                            \\tDetected in spec() [HESpec.hs" ++ (show index) ++ " line 1122]\n\
                            \\tCustom HDF error report 1.23\n"
                _             <-           he_clear
                _             <-           he_push DFE_DENIED "aaa" ("HESpec.hs" ++ show (index :: Int)) 11
                _             <-           he_push DFE_DENIED "bbb" ("HESpec.hs" ++ show (index :: Int)) 22
                _             <-           he_push DFE_DENIED "ccc" ("HESpec.hs" ++ show (index :: Int)) 33
                _             <-           he_push DFE_DENIED "ddd" ("HESpec.hs" ++ show (index :: Int)) 44
                _             <-           he_push DFE_DENIED "eee" ("HESpec.hs" ++ show (index :: Int)) 55
                _             <-           he_push DFE_DENIED "fff" ("HESpec.hs" ++ show (index :: Int)) 66
                _             <-           he_push DFE_DENIED "ggg" ("HESpec.hs" ++ show (index :: Int)) 77
                _             <-           he_push DFE_DENIED "hhh" ("HESpec.hs" ++ show (index :: Int)) 88
                _             <-           he_push DFE_DENIED "iii" ("HESpec.hs" ++ show (index :: Int)) 99
                _             <-           sd_start "NULL" HDFRead
                _             <-           he_push DFE_DENIED "spec" ("HESpec.hs" ++ show (index :: Int)) 1122
                _             <-           he_report "Custom HDF error report %4.2f" (1.23 :: CDouble)
                _             <-           performGC
                _             <-           newCString $ replicate 4096 'x'
                _             <-           he_print logFileName
                contents      <- (unlines . take 3 . lines) <$> readFile logFileName
                contents `shouldBe` expectedTrace
        -}

