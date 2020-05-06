#include <hdf.h>
#include <mfhdf.h>
#include <stdlib.h>

intn wrp_SDsetchunk(int32 sdsid, HDF_CHUNK_DEF *chunk_def, int32 flags) {
    return SDsetchunk(sdsid, *chunk_def, flags);
}

static const char **error_stack_file_names = NULL;
static const char **file_names_to_clear = NULL;

static size_t num_file_names = 0;
static size_t max_file_names = 1024;

const char ** wrp_HEpush(hdf_err_code_t error_code, const char *function_name, const char *file_name, intn line, size_t *const num_unused_names) {

    if (!error_stack_file_names) {
        error_stack_file_names = (const char **)malloc(max_file_names*sizeof(char *));
        file_names_to_clear    = (const char **)malloc(max_file_names*sizeof(char *));
    }

    if (num_file_names == max_file_names) {
        max_file_names += 1024;
        error_stack_file_names = (const char **)realloc(error_stack_file_names, max_file_names*sizeof(char **));
        file_names_to_clear    = (const char **)realloc(file_names_to_clear,    max_file_names*sizeof(char **));
    }
    error_stack_file_names[num_file_names] = file_name;
    ++num_file_names;

    HEpush(error_code, function_name, file_name, line);

    if(num_file_names > (size_t)error_top) {
    /* Error stack could contain at most the number of custom errors
       which is equal to the current error stack size. If we store
       more custom errors than could be held by the current error stack
       it means the error stack was cleared and we could safely deallocate
       the memory taken by the old messages. */

        *num_unused_names = num_file_names - (size_t)error_top;
        for(size_t i = 0; i < *num_unused_names; ++i) {
            file_names_to_clear[i] = error_stack_file_names[i];
        }

        size_t k = 0;
        for(size_t i = *num_unused_names; i < num_file_names; ++i) {
            error_stack_file_names[k] = error_stack_file_names[i];
            ++k;
        }
        num_file_names = k;
    } else {
        *num_unused_names = 0;
    }

    return file_names_to_clear;
}

const char **wrp_HEclear(size_t *num_stored_file_names) {
    *num_stored_file_names = num_file_names;
    num_file_names = 0;

    HEclear();
    return error_stack_file_names;
}