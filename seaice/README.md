# Sea ice binary to intermediate
Fortran, Python and NCL code to convert Bootstrap sea ice binary files
to (1) WRF intermediate format and (2) netCDF.

read_data.f90: Fortran to do the conversion to intermediate format

Creates 6h output from daily input.  Compile with "ifort -o read_data read_data.f90".

make_seaice: Python wrapper to run read_data

Dynamically creates namelist.input for read_data

run_make_seaice: Python wrapper to run make_seaice over a time range

icebin2nc.ncl: NCL script to convert binary file to netCDF

interp/

Utilities to interpolate missing days

*.dat files

Binary latitude/longitude files for Bootstrap (EASE) grids
