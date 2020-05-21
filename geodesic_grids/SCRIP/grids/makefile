#
# Makefile for interpolation code
# CVS:$Id: makefile,v 1.3 2000/04/19 22:05:59 pwjones Exp $
#
#COMP = xlf
#COMP = f90
COMP = pgf90
#FLAGS = -O3 -64 -r10000 -I/usr/local/include 
#FLAGS = -g
FLAGS = -O3 -I/usr/local/include -Mextend
LIB  = -lnetcdf 
INCL = 
SRCDIR  = ../source
EXEDIR  = .
OBJ  = \
	kinds_mod.o \
	constants.o \
	iounits.o \
	netcdf.o

all: convertPOPT convertgauss create_latlon

convertold: $(OBJ) convert_old.o
	$(COMP) $(FLAGS) $(OBJ) convert_old.o $(LIB) -o $(EXEDIR)/convertold

convertPOPT: $(OBJ) convertPOPT.o
	$(COMP) $(FLAGS) $(OBJ) convertPOPT.o $(LIB) -o $(EXEDIR)/convertPOPT

NCEP_gauss_T62: $(OBJ) NCEP_gauss_T62.o
	$(COMP) $(FLAGS) $(OBJ) NCEP_gauss_T62.o $(LIB) -o $(EXEDIR)/NCEP_gauss_T62

ECHAM5_gauss_T63: $(OBJ) ECHAM5_gauss_T63.o
	$(COMP) $(FLAGS) $(OBJ) ECHAM5_gauss_T63.o $(LIB) -o $(EXEDIR)/ECHAM5_gauss_T63

convertgauss: $(OBJ) convertgauss.o
	$(COMP) $(FLAGS) $(OBJ) convertgauss.o $(LIB) -o $(EXEDIR)/convertgauss

create_latlon: $(OBJ) create_latlon.o
	$(COMP) $(FLAGS) $(OBJ) create_latlon.o $(LIB) -o $(EXEDIR)/create_latlon

kinds_mod.o: $(SRCDIR)/kinds_mod.f $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/kinds_mod.f

constants.o: $(SRCDIR)/constants.f kinds_mod.o $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/constants.f

iounits.o: $(SRCDIR)/iounits.f kinds_mod.o constants.o $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/iounits.f

netcdf.o: $(SRCDIR)/netcdf.f kinds_mod.o constants.o $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/netcdf.f

convert_old.o: convert_old.f kinds_mod.o constants.o iounits.o netcdf.o $(INCL)
	$(COMP) $(FLAGS) -c convert_old.f

convertPOPT.o: convertPOPT.f kinds_mod.o constants.o iounits.o netcdf.o $(INCL)
	$(COMP) $(FLAGS) -c convertPOPT.f

ECHAM5_gauss_T63.o: ECHAM5_gauss_T63.f kinds_mod.o constants.o iounits.o netcdf.o $(INCL)
	$(COMP) $(FLAGS) -c ECHAM5_gauss_T63.f

NCEP_gauss_T62.o: NCEP_gauss_T62.f kinds_mod.o constants.o iounits.o netcdf.o $(INCL)
	$(COMP) $(FLAGS) -c NCEP_gauss_T62.f

convertgauss.o: convertgauss.f kinds_mod.o constants.o iounits.o netcdf.o $(INCL)
	$(COMP) $(FLAGS) -c convertgauss.f

create_latlon.o: create_latlon.f kinds_mod.o constants.o iounits.o netcdf.o $(INCL)
	$(COMP) $(FLAGS) -c create_latlon.f

clean: 
	/bin/rm *.o *.mod

