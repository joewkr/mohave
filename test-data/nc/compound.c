#include <stdio.h>
#include <stddef.h>
#include <netcdf.h>
#include <stdlib.h>

static int check(int err,int line)
{
    if(err != NC_NOERR) {
        fprintf(stderr,"fail (%d): %s\n", line, nc_strerror(err));
        fflush(stderr);
        exit(1);
    }
    return NC_NOERR;
}

#define CHECK(x) check(x,__LINE__)

struct S {
    char c;
    double d;
} __attribute__((packed));

struct D {
    char c;
    double d;
};

int main(void)
{
    struct S data_s = {'y', 13.12};
    struct D data_d = {'n', 14.75};

    int ncid, varid1, varid2;
    nc_type typeid1, typeid2;

    CHECK(nc_create("compound.nc", NC_CLOBBER | NC_NETCDF4, &ncid));

    CHECK(nc_def_compound(ncid, sizeof(struct S), "S", &typeid1));
    CHECK(nc_insert_compound(ncid, typeid1, "c", offsetof(struct S,c), NC_CHAR));
    CHECK(nc_insert_compound(ncid, typeid1, "d", offsetof(struct S,d), NC_DOUBLE));
    CHECK(nc_def_var(ncid, "scalar_variable_S", typeid1, 0, NULL, &varid1));

    CHECK(nc_def_compound(ncid, sizeof(struct D), "D", &typeid2));
    CHECK(nc_insert_compound(ncid, typeid2, "d", offsetof(struct D,d), NC_DOUBLE));
    CHECK(nc_insert_compound(ncid, typeid2, "c", offsetof(struct D,c), NC_CHAR));
    CHECK(nc_def_var(ncid, "scalar_variable_D", typeid2, 0, NULL, &varid2));
    CHECK(nc_enddef(ncid));

    CHECK(nc_put_var(ncid, varid1, &data_s));
    CHECK(nc_put_var(ncid, varid2, &data_d));

    CHECK(nc_close(ncid));
}
