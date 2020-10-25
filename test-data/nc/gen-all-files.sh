#!/bin/sh -x

cd test-data/nc

for file in `ls *.cdl`; do
  nc_name=`basename $file .cdl`.nc
  ncgen -k 'netCDF-4' -o $nc_name $file
done

