!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  1/20/06, dbr, expanded lat range to get +/- 90 deg centered boxes.
!                included options to either have top/bot row extend
!                beyond +/-90 (full 2.5 deg box) or be half boxes.  the
!                latter has center on edge, which could be a problem.
!                NOTE:  NCEP starts at North Pole, *not* South Pole
!  1/13/06, dbr, modified to create a 2.5 deg grid (for NCEP)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This program creates a remapping grid file for a lat/lon grid.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: create_latlon.f,v 1.1 2000/04/19 22:05:58 pwjones Exp $
!
!     Copyright (c) 1997, 1998 the Regents of the University of 
!       California.
!
!     Unless otherwise indicated, this software has been authored
!     by an employee or employees of the University of California,
!     operator of the Los Alamos National Laboratory under Contract
!     No. W-7405-ENG-36 with the U.S. Department of Energy.  The U.S.
!     Government has rights to use, reproduce, and distribute this
!     software.  The public may copy and use this software without
!     charge, provided that this Notice and any statement of authorship
!     are reproduced on all copies.  Neither the Government nor the
!     University makes any warranty, express or implied, or assumes
!     any liability or responsibility for the use of this software.
!
!***********************************************************************

      program create_latlon

!-----------------------------------------------------------------------
!
!     This file creates a remapping grid file for a Gaussian grid
!
!-----------------------------------------------------------------------

      use kinds_mod
      use constants
      use iounits
      use netcdf_mod

      implicit none

!-----------------------------------------------------------------------
!
!     variables that describe the grid
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), parameter ::
     &             nx = 144, ny = 73,
     &             grid_size = nx*ny,
     &             grid_rank = 2,
     &             grid_corners = 4

! *** OPTION
! use following to get 73 rows of full boxes
      character(char_len), parameter :: 
     &             grid_name = '2.5 degree NCEP Grid',
     &             grid_desc = 'Note: top/bottom boxes go beyond +/- 90',
     &             grid_file_out = 'NCEP_2.5deg_grid.nc'

! *** OPTION
! use following to get 71 rows of full boxes plus top/bottom rows as half boxes
!      character(char_len), parameter :: 
!     &             grid_name = '2.5 degree NCEP Grid',
!     &             grid_desc = 'Note: half boxes in top/bottom rows, with center along edge',
!     &             grid_file_out = 'NCEP_2.5deg_grid_halfbox.nc'

      integer (kind=int_kind), dimension(grid_rank) ::
     &             grid_dims

! half-box adjustment to get rows centered correctly on model points
      real (kind=dbl_kind), parameter ::
     &             grid_offset = 1.25

!-----------------------------------------------------------------------
!
!     grid coordinates and masks
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), dimension(grid_size) ::
     &             grid_imask

      real (kind=dbl_kind), dimension(grid_size) ::
     &             grid_center_lat,  ! lat/lon coordinates for
     &             grid_center_lon   ! each grid center in degrees

      real (kind=dbl_kind), dimension(grid_corners,grid_size) ::
     &             grid_corner_lat,  ! lat/lon coordinates for
     &             grid_corner_lon   ! each grid corner in degrees

!-----------------------------------------------------------------------
!
!     other local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: i, j, iunit, atm_add

      integer (kind=int_kind) ::
     &        ncstat,            ! general netCDF status variable
     &        nc_grid_id,        ! netCDF grid dataset id
     &        nc_gridsize_id,    ! netCDF grid size dim id
     &        nc_gridcorn_id,    ! netCDF grid corner dim id
     &        nc_gridrank_id,    ! netCDF grid rank dim id
     &        nc_griddims_id,    ! netCDF grid dimension size id
     &        nc_grdcntrlat_id,  ! netCDF grid center lat id
     &        nc_grdcntrlon_id,  ! netCDF grid center lon id
     &        nc_grdimask_id,    ! netCDF grid mask id
     &        nc_grdcrnrlat_id,  ! netCDF grid corner lat id
     &        nc_grdcrnrlon_id   ! netCDF grid corner lon id

      integer (kind=int_kind), dimension(2) ::
     &        nc_dims2_id        ! netCDF dim id array for 2-d arrays

      real (kind=dbl_kind) :: dlon, minlon, maxlon, centerlon,
     &                        dlat, minlat, maxlat, centerlat

!-----------------------------------------------------------------------
!
!     compute longitudes and latitudes of cell centers and corners.
!
!-----------------------------------------------------------------------

      grid_dims(1) = nx
      grid_dims(2) = ny

      dlon = 360./nx
! added 2.5 deg to get boxes properly centered over poles
      dlat = 182.5/ny

      do j=1,ny

! NCEP latitudes:  north-to-south
        minlat = 90_dbl_kind - (j-1)*dlat + grid_offset
        maxlat = 90_dbl_kind -  j   *dlat + grid_offset
        centerlat = minlat + half*dlat - 2*grid_offset

! ERA-40 latitudes:  north-to-south
!        minlat = -90._dbl_kind + (j-1)*dlat - grid_offset
!        maxlat = -90._dbl_kind +  j   *dlat - grid_offset
!        centerlat = minlat + half*dlat


! *** OPTION
! the following forces "half-boxes" in top/bottom rows
! doing this, however, puts the grid_center_lat on the edge
! uncomment when creating *_halfbox.nc
! 
! NCEP style grid
!				if (minlat .gt. 90 ) then
!				  minlat = 90
!				end if
!				if (maxlat .lt. -90 ) then
!				  maxlat = -90
!				end if

! ERA-40 style grid
!				if (minlat .lt. -90 ) then
!				  minlat = -90
!				end if
!				if (maxlat .gt. 90 ) then
!				  maxlat = 90
!				end if

        do i=1,nx
          centerlon = (i-1)*dlon
          minlon = centerlon - half*dlon
          maxlon = centerlon + half*dlon

          atm_add = (j-1)*nx + i

          grid_center_lat(atm_add  ) = centerlat
          grid_corner_lat(1,atm_add) = minlat
          grid_corner_lat(2,atm_add) = maxlat
          grid_corner_lat(3,atm_add) = maxlat
          grid_corner_lat(4,atm_add) = minlat

          grid_center_lon(atm_add  ) = centerlon
          grid_corner_lon(1,atm_add) = minlon
          grid_corner_lon(2,atm_add) = minlon
          grid_corner_lon(3,atm_add) = maxlon
          grid_corner_lon(4,atm_add) = maxlon

! for ERA-40 type grids
!          grid_center_lat(atm_add  ) = centerlat
!          grid_corner_lat(1,atm_add) = minlat
!          grid_corner_lat(2,atm_add) = minlat
!          grid_corner_lat(3,atm_add) = maxlat
!          grid_corner_lat(4,atm_add) = maxlat
!
!          grid_center_lon(atm_add  ) = centerlon
!          grid_corner_lon(1,atm_add) = minlon
!          grid_corner_lon(2,atm_add) = maxlon
!          grid_corner_lon(3,atm_add) = maxlon
!          grid_corner_lon(4,atm_add) = minlon
        end do
      end do

!-----------------------------------------------------------------------
!
!     define mask
!
!-----------------------------------------------------------------------

      grid_imask = 1

!-----------------------------------------------------------------------
!
!     set up attributes for netCDF file
!
!-----------------------------------------------------------------------

      !***
      !*** create netCDF dataset for this grid
      !***

      ncstat = nf_create (grid_file_out, NF_CLOBBER,
     &                    nc_grid_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_att_text (nc_grid_id, NF_GLOBAL, 'title',
     &                          len_trim(grid_name), grid_name)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_att_text (nc_grid_id, NF_GLOBAL, 'description',
     &                          len_trim(grid_desc), grid_desc)
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid size dimension
      !***

      ncstat = nf_def_dim (nc_grid_id, 'grid_size', grid_size, 
     &                     nc_gridsize_id)
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid corner dimension
      !***

      ncstat = nf_def_dim (nc_grid_id, 'grid_corners', grid_corners, 
     &                     nc_gridcorn_id)
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid rank dimension
      !***

      ncstat = nf_def_dim (nc_grid_id, 'grid_rank', grid_rank, 
     &                     nc_gridrank_id)
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid dimension size array
      !***

      ncstat = nf_def_var (nc_grid_id, 'grid_dims', NF_INT,
     &                     1, nc_gridrank_id, nc_griddims_id)
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid center latitude array
      !***

      ncstat = nf_def_var (nc_grid_id, 'grid_center_lat', NF_DOUBLE,
     &                     1, nc_gridsize_id, nc_grdcntrlat_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_att_text (nc_grid_id, nc_grdcntrlat_id, 'units',
     &                          7, 'degrees')
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid center longitude array
      !***

      ncstat = nf_def_var (nc_grid_id, 'grid_center_lon', NF_DOUBLE,
     &                     1, nc_gridsize_id, nc_grdcntrlon_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_att_text (nc_grid_id, nc_grdcntrlon_id, 'units',
     &                          7, 'degrees')
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid mask
      !***

      ncstat = nf_def_var (nc_grid_id, 'grid_imask', NF_INT,
     &                     1, nc_gridsize_id, nc_grdimask_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_att_text (nc_grid_id, nc_grdimask_id, 'units',
     &                          8, 'unitless')
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid corner latitude array
      !***

      nc_dims2_id(1) = nc_gridcorn_id
      nc_dims2_id(2) = nc_gridsize_id

      ncstat = nf_def_var (nc_grid_id, 'grid_corner_lat', NF_DOUBLE,
     &                     2, nc_dims2_id, nc_grdcrnrlat_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_att_text (nc_grid_id, nc_grdcrnrlat_id, 'units',
     &                          7, 'degrees')
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid corner longitude array
      !***

      ncstat = nf_def_var (nc_grid_id, 'grid_corner_lon', NF_DOUBLE,
     &                     2, nc_dims2_id, nc_grdcrnrlon_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_att_text (nc_grid_id, nc_grdcrnrlon_id, 'units',
     &                          7, 'degrees')
      call netcdf_error_handler(ncstat)

      !***
      !*** end definition stage
      !***

      ncstat = nf_enddef(nc_grid_id)
      call netcdf_error_handler(ncstat)

!-----------------------------------------------------------------------
!
!     write grid data
!
!-----------------------------------------------------------------------

      ncstat = nf_put_var_int(nc_grid_id, nc_griddims_id, grid_dims)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_var_int(nc_grid_id, nc_grdimask_id, grid_imask)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_var_double(nc_grid_id, nc_grdcntrlat_id, 
     &                           grid_center_lat)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_var_double(nc_grid_id, nc_grdcntrlon_id, 
     &                           grid_center_lon)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_var_double(nc_grid_id, nc_grdcrnrlat_id, 
     &                           grid_corner_lat)
      call netcdf_error_handler(ncstat)

      ncstat = nf_put_var_double(nc_grid_id, nc_grdcrnrlon_id, 
     &                           grid_corner_lon)
      call netcdf_error_handler(ncstat)

      ncstat = nf_close(nc_grid_id)
      call netcdf_error_handler(ncstat)

!-----------------------------------------------------------------------

      end program create_latlon

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
