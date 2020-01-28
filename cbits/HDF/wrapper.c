#include <hdf.h>
#include <mfhdf.h>

intn wrp_SDsetchunk(int32 sdsid, HDF_CHUNK_DEF *chunk_def, int32 flags) {
    return SDsetchunk(sdsid, *chunk_def, flags);
}

void wrp_HEclear() {
    HEclear();
}
