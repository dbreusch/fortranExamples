
	PROGRAM driver

!----------------------------------------------------------------------
! 7-13-99
! written by the CSU General Circulation Modeling Group
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! NOTE TO ALL USERS
!   search for the string "***", it locates all places where you
!   may want to make modifications for your application
!
! NOTE: the output of this routine is two data files call
!   test8.nc and test4.nc. Each contain surface orography interpolated
!   for the geodesic 02562 grid to the CSU 4x5 lat-lon grid. They 
!   differ only in the precision of the calculation (real*8 or real*4).
!   Please compare your results to test8.nc.validate 
!   and test4.nc.validate.
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! VARIABLE DEFINITION
!
!  remapfile	: netCDF file containing the remapping weights
!  src_size	: number of grid cells on the grid which the data
!		    is interpolated FROM, set dynamically
!  dst_size	: number of grid cells on the grid which the data
!                   is interpolated TO, set dynamically
!----------------------------------------------------------------------

	use interpolate
	use getdata

        implicit none


	real*4, dimension(:), allocatable :: xin4
	real*4, dimension(:), allocatable :: xout4
	real*4, dimension(:), allocatable :: xerr4

	real*8, dimension(:), allocatable :: xin8
	real*8, dimension(:), allocatable :: xout8
	real*8, dimension(:), allocatable :: xerr8

	integer :: src_size, dst_size

        character(len=80) :: remapfile, outfile

!----------------------------------------------------------------------
! ***
! USERS: remapfile contains the data which allows interplolation 
!   between grids. The current setting is to remap data which is
!   defined on the 2562 spherical geodesic grid onto the CSU 4 deg
!   by 5 deg grid.
!----------------------------------------------------------------------
!        remapfile = "rmp_C02562_to_csu_72x44_conserv.nc"
        remapfile = "rmp_csu_72x44_to_C10242_conserv.nc"
!        outfile = "test8.nc"
        outfile = "geotest.nc"

!----------------------------------------------------------------------
!
! This demonstrates the sequence of calls needed to interpolate:
!  - interp_init (remapping_file, source_grid_size, destination_grid_size)
!      Initializes the interpolation module.
!      Returns the size of the source and destination grids.
!
!  - interp (input_data, output_data)
!      Interpolates the data.
!
!  - interp_out (output_file_name, output_variable_name, output_data)
!      Outputs the grid and the interpolated data.  
!      Nothing is written to the output file until interp_out is called.
!
!  - interp_done 
!      Tells the interpolation module that no more data will
!      be output to this file. (so it can deallocate some variables)
!----------------------------------------------------------------------


!----------------------------------------------------------------------
! initialize interpolation routine (returns grid sizes so that
! variables to store data to be interpolated may be allocated)
!----------------------------------------------------------------------
        call interp_init(remapfile, src_size, dst_size)
        allocate (xin8(src_size), xout8(dst_size))

!----------------------------------------------------------------------
! ***
! call the read routine
! USERS : this sample file "C02562.orog.nc" contains surface orography, zs.
! if you want to interpolate other data, replace the "call readdata"
! with your own hook, put the data that you want to interpolate
! into xin8 and proceed
!----------------------------------------------------------------------
!        call readdata("C02562.orog.nc", "zs", xin8)
        call readdata("test8.nc", "zs", xin8)
        call interp(xin8, xout8)
        call interp_out(outfile, "zs", xout8)

!----------------------------------------------------------------------
! ***
! repeat calls to read, interp and interp_out to store
! more than one variable in the output netcdf file.
! USERS: you can interpolate as many data fields as you wish, they
! will all get packed into the same netCDF output file
! uncomment the next 3 lines and use your own hook to get the data
! you wish to be intepolated
!----------------------------------------------------------------------
!       call readdata("C02562.binary.nc", "some_other_variable", xin8)
!       call interp(xin8, xout8)
!       call interp_out(outfile, "dummy2", xout8)

!----------------------------------------------------------------------
! remember to finish with a call to interp_done, so that allocated 
! memory is freed properly.
!----------------------------------------------------------------------
        call interp_done()


!----------------------------------------------------------------------
! same as above, using real*4 interface
!----------------------------------------------------------------------
!        call interp_init(remapfile, src_size, dst_size)
!        allocate (xin4(src_size), xout4(dst_size))
!
!        xin4 = xin8
!        call interp(xin4, xout4)
!        call interp_out("test4.nc", "zs", xout4)
!
!        call interp_done()


        END PROGRAM driver
