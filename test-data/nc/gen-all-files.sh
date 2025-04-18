#!/bin/sh -x

cd test-data/nc

for file in `ls *.cdl`; do
  nc_name=`basename $file .cdl`.nc
  ncgen -k 'netCDF-4' -o $nc_name $file
done

for file in `ls *.cdl3`; do
  nc_name=`basename $file .cdl3`.nc
  ncgen -k 'nc3' -o $nc_name $file
done

cc compound.c `nc-config --cflags --libs` -o gen-compound && ./gen-compound

