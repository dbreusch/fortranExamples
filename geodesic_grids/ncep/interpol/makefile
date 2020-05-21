#
# Makefile for interpolation program
#

#
# set to a Fortran 90 compiler
#
F90 = pgf90
F90FLAGS = -O3 -lpgf90

#
# compiler-generated module file suffix
#
# Used to remove old module files for "make clean".
#
MOD_SUFF = mod

#
# change to reflect your system's netcdf installation:
#
#
# location of netcdf library (libnetcdf.a)
#
NETCDF_LIB = /usr/local/lib

#
# location of Fortran netcdf include file (netcdf.inc)
#
NETCDF_INC = /usr/local/include

#
# replace this with your read implementation - 
#
# this should be a file that contains one publicly available 
# subroutine used to read the input data.
#
# see sample_read.f90 for an example
#
# Note: make must be able to figure out how to build this object
#       from your source file... this may require that you provide
#       a make rule if the default rules are insufficient.
#
READ_DATA_OBJ = sample_read.o



########################################################################
#
# end of configuration.
# you should not have to modify anything below this line.
#
########################################################################


OBJECTS = netcdf.o \
          interpolate.o \
	  $(READ_DATA_OBJ) \
	  driver.o

.SUFFIXES: .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<


all : interpolate


interpolate: $(OBJECTS)
	$(F90) $(F90FLAGS) -o $@ $(OBJECTS) -L$(NETCDF_LIB) -lnetcdf

clean:
	rm -f $(OBJECTS) *.$(MOD_SUFF) interp

