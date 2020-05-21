# Geodesic grid interpolation
Files from a 2006 project to use the SCRIP package to interpolate from
standard lat/lon grids to geodesic (primarily hexagonal) grids.

main directory:
grids/ interpol/ ncep/ SCRIP/

grids:
--- files related to various grids
    KML, netCDF, lat/lon-as-text
    plots (Postscript, Illustrator, TIFF)
    utility scripts, Matlab scripts/mat-files

interpol:
--- sample code, grids, etc., for interpolation
interpolate.f90 driver.f90 sample_read.f90 netcdf.f90
interpolate
makefile README
C02562.orog.nc
rmp_C02562_to_csu_72x44_conserv.nc
test4.nc test4.nc.validate test8.nc test8.nc.validate
geotest.nc geotest.ncl

ncep:
--- working directory for NCEP
interpol/ ncep.fields/ test.data/

ncep/interpol:
--- customized code for interpolation of NCEP files
interpolate.f90 driver.f90 sample_read.f90 netcdf.f90
interpolate_in
interpolate
makefile README

ncep/ncep.fields:
-- Postscript output of test datasets
air.1995.daily.0131.700.ps
air.1996.daily.0410.700.ps
air.20000629.700.ps
air.20000927.500.ps
air.2000.daily.0101.700.ps
air.2000.daily.0131.700.ps
air.2000.daily.0901.700.ps
air.sig995.1993.daily.0222.ps
air.sig995.1993.daily.0222.sfc.ps
air.sig995.19960118.ps
uwnd.19960118.700.ps

ncep/test.data:
--- netCDF and Postscript output of test datasets
--- NCL script to plot test datasets
README
plot_T_2d_g.ncl plot_T_3d_g.ncl
air.1995.daily.0131.700.regrid.ps
air.1995.daily.700.regrid.nc
air.1996.daily.0410.700.regrid.ps
air.1996.daily.700.nc
air.1996.daily.700.regrid.nc
air.20000629.700.nc
air.20000629.700.regrid.nc
air.20000629.700.regrid.ps
air.2000092700.500.regrid.nc
air.20000927.500.nc
air.20000927.500.regrid.nc
air.20000927.500.regrid.ps
air.2000.daily.0101.700.regrid.ps
air.2000.daily.0131.700.regrid.ps
air.2000.daily.0901.700.regrid.ps
air.2000.daily.700.nc
air.2000.daily.700.regrid.nc
air.sig995.1993.daily.0222.regrid.ps
air.sig995.1993.daily.regrid.nc
air.sig995.19960118.nc
air.sig995.19960118.regrid.nc
air.sig995.19960118.regrid.ps
air.sig995.19960118.sfc.regrid.nc
uwnd.19960118.700.nc
uwnd.19960118.700.regrid.nc
uwnd.19960118.700.regrid.ps

SCRIP:
--- Geodesic grid main directory
doc/ grids/ remap/ source/
scrip
--- namelist files for scrip
scrip_in scrip_in_NCEP_2.5 scrip_in_orig scrip_in_test
--- remap file validator
scrip_test scrip_test_in
bugs

SCRIP/doc:
--- SCRIP documentation
SCRIPusers.pdf SCRIPusers.ps SCRIPusers.tex

SCRIP/grids:
--- source and utility files
convertgauss.f convertPOPT.f create_latlon.f create_latlon_orig.f
makefile README
plot_grids.ncl
find_neigh.m

--- geodesic grids
C02562.global.nc C10242.global.nc C40962.global.nc
geodesic_neighbors.txt
grid_geod_Merc.ps.gz grid_geod_Moll.ps.gz

--- NCEP grids
NCEP_2.5deg_grid.nc NCEP_2.5deg_grid_halfbox.nc
ncep_neighbors.txt
grid_NCEP_Merc.ps.gz grid_NCEP_Moll.ps.gz

--- other grids
csu_72x44.global.nc
ECMWF_2.5x2.5.global.nc
ERA2.5deg_grid.nc ERA2.5deg_grid_v2.nc
T42.global.nc

--- remap files
remap_grid_POP43.nc
remap_grid_T42.nc
remap_grid_T62_NCEP.nc

SCRIP/remap:
--- scrip-created grid remapping netCDF files
rmp_C02562_to_csu_72x44_conserv.nc
rmp_C10242_to_2.5x2.5_conserv.nc
rmp_C10242_to_csu_72x44_conserv.nc
rmp_C10242_to_NCEP_2.5deg_conserv.nc
rmp_C10242_to_NCEP_T62_conserv.nc
rmp_csu_72x44_to_C02562_conserv.nc
rmp_csu_72x44_to_C10242_conserv.nc
rmp_NCEP_2.5deg_to_C10242_conserv.nc
rmp_NCEP_T62_to_C10242_conserv.nc
rmp_POP43_to_T42_conserv.nc
rmp_T42_to_POP43_conserv.nc

SCRIP/source:
--- source files for scrip
