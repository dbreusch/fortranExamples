!-----------------------------------------------------------------------
!
	MODULE interpolate
!
!-----------------------------------------------------------------------
! Purpose:
!   Contains variables and subroutines to interpolate data
!
! Scope:
!   Public Subroutines:
!     interp_init    - reads grid addresses and remap matrix from  
!                      netcdf remapping file 
!     interp         - performs interpolation.
!     interp_out     - outputs given data on destination grid
!     interp_done    - deallocates arrays allocated in interp_init
!   Public Variables:
!     none
!   Private Subroutines:
!     get_dst_grid_info - reads information about destination grid
!                         from the remapping file
!     define_out_grid_info - defines header data for the output netcdf file
!     write_out_grid_info - writes header data to output netcdf file
!   Modules used:
!     netcdf_mod - for netcdf interface and error handler
!
! Bugs:
!     1) Arbitrary limits:
!        Maximum file name length = 255 characters (remapping file)
!        Maximum number dimensions = 9 (data variable output)
!
!-----------------------------------------------------------------------

        use netcdf
        implicit none
	private

	integer :: num_links
	integer :: num_wgts
        integer :: src_grid_size
        integer :: dst_grid_size
        integer :: dst_grid_rank
        integer :: dst_grid_corners

        integer :: out_sizeID, out_rankID, out_cornerID

 	integer :: grid_dims_id,                                       &
                   grid_center_lat_id,                                 & 
                   grid_center_lon_id,                                 &
                   grid_imask_id,                                      &
                   grid_corner_lat_id,                                 &
                   grid_corner_lon_id


! save remap file name to use in get_dst_grid_info
        integer, parameter :: MAX_FILENAME=255
        character(len=MAX_FILENAME) :: remapfile

        logical :: first

!        logical :: normalized   ! not used yet...

	real*8, dimension(:,:), allocatable :: remap_matrix
	integer, dimension(:), allocatable :: src_address,             &
                                              dst_address

        integer, dimension(:), allocatable :: grid_dim_ids
        integer, dimension(:), allocatable :: dst_grid_dims
	integer, dimension(:), allocatable :: dst_grid_imask
	real*8, dimension(:), allocatable :: dst_grid_center_lat,      &
       	                                     dst_grid_center_lon
        real*8, dimension(:,:), allocatable :: dst_grid_corner_lat,    &
                                               dst_grid_corner_lon


! conditionally allocated variables - be careful when deallocating
!	real*8, dimension(:), allocatable :: dst_grid_area 
!	real*8, dimension(:), allocatable :: dst_grid_frac


! this provides a single interface for both real and 
! double precision data interpolations
	INTERFACE interp
	   MODULE PROCEDURE interp4
	   MODULE PROCEDURE interp8
        END INTERFACE 

        INTERFACE interp_out
           MODULE PROCEDURE interp4_out
           MODULE PROCEDURE interp8_out
        END INTERFACE

        public :: interp_init, interp, interp_out, interp_done

	CONTAINS


!-----------------------------------------------------------------------
!
	SUBROUTINE interp_init (filename, src_size, dst_size)
!
!-----------------------------------------------------------------------
! Purpose: 
!   Gets address and remap values from netcdf file (remapfile)
!
! Scope: public subroutine
!   Module variables set:
!     first
!     remapfile
!     src_grid_size 
!     dst_grid_size 
!     num_links 
!     num_wgts
!     src_address
!     dst_address
!     remap_matrix
!   Arguments:
!     filename - name of the remapping file
!     src_size - size of the source grid as defined in the remapping file
!     dst_size - size of the destination grid as defined in the remapping file
!
! Bugs:
!   1) length of remapping file name is limited to MAX_FILENAME characters
!
!-----------------------------------------------------------------------

        character(len=*), intent(in) :: filename   
        integer, intent(out) :: src_size
        integer, intent(out) :: dst_size
 
	integer :: fileID
	integer :: num_links_id,                                       &
                   num_wgts_id,                                        &
      		   src_addr_id,                                        &
                   dst_addr_id,                                        &
                   remap_mat_id

	integer :: status, ncid, i


! if init is called, this is the first time out
        first = .true.

! set globally available file to argument
        if (len_trim(filename) > MAX_FILENAME) then
           print *, "Warning: file name ", filename,                   &
                    " is longer than ", MAX_FILENAME, " characters."
        end if
        remapfile = filename

        status = nf_open (remapfile, NF_NOWRITE, fileID)
	call handle_error (status, "file = "//remapfile)

! get dimension sizes
!    grid sizes are needed for interpolation bound checking
        status = nf_inq_dimid (fileID, "src_grid_size", ncid)
	call handle_error (status, "src_grid_size")
	status = nf_inq_dimlen (fileID, ncid, src_grid_size)
	call handle_error (status, "src_grid_size")

        status = nf_inq_dimid (fileID, "dst_grid_size", ncid)
	call handle_error (status, "dst_grid_size")
	status = nf_inq_dimlen (fileID, ncid, dst_grid_size)
	call handle_error (status, "dst_grid_size")

        status = nf_inq_dimid (fileID, "num_links", num_links_id)
	call handle_error (status, "num_links")
	status = nf_inq_dimlen (fileID, num_links_id, num_links)
	call handle_error (status)

        status = nf_inq_dimid (fileID, "num_wgts", num_wgts_id)
	call handle_error (status, "num_wgts")
	status = nf_inq_dimlen (fileID, num_wgts_id, num_wgts)
	call handle_error (status)


! allocate space for data arrays
	allocate (src_address(num_links))
	allocate (dst_address(num_links))
	allocate (remap_matrix(num_wgts, num_links))


! get the remap information
        status = nf_inq_varid (fileID, "src_address", src_addr_id)
	call handle_error (status, "src_address")
	status = nf_get_var_int (fileID, src_addr_id, src_address)
	call handle_error (status, "src_address")

	status = nf_inq_varid (fileID, "dst_address", dst_addr_id)
	call handle_error (status, "dst_address")
	status = nf_get_var_int (fileID, dst_addr_id, dst_address)
	call handle_error (status, "dst_address")

	status = nf_inq_varid (fileID, "remap_matrix", remap_mat_id)
	call handle_error (status, "remap_matrix")
	status = nf_get_var_double (fileID, remap_mat_id, remap_matrix)
	call handle_error (status, "remap_matrix")


! FIXME - get normalization attribute here...

! done with remap file 
	status = nf_close (fileID)
	call handle_error (status)

	src_size = src_grid_size
	dst_size = dst_grid_size


        END SUBROUTINE interp_init




!-----------------------------------------------------------------------
!
	SUBROUTINE interp4(xin, xout, src_grad_lat, src_grad_lon)
!
!-----------------------------------------------------------------------
! Purpose:
!   Interpolates real*4 input data (xin) 
!   
!  Scope: private (with public module interface)
!    Module variables used:
!       src_grid_size
!       dst_grid_size
!       num_links
!       num_wgts
!       dst_address
!       src_address
!       remap_matrix
!    Arguments:
!       xin: input data (defined on source grid)
!       xout: output data (defined on destination grid)
!       src_grad_lat: gradient of input data (on source grid)
!       src_grad_lon: gradient of input data (on source grid)
!
! Bugs:
!    1) cannot be called before interp_init
!
!-----------------------------------------------------------------------

	real*4, dimension(:), intent(in) :: xin
	real*4, dimension(:), intent(out) :: xout

	real*4, dimension(:), intent(in), optional :: src_grad_lat
	real*4, dimension(:), intent(in), optional :: src_grad_lon

        integer :: i

! check arrays for size consistency
        if (size(xin) /= src_grid_size) then
           print *, "Interpolation warning: input grid size does not match:"
           print *, "   Input (xin) size    = ", size(xin)
           print *, "   Expected size       = ", src_grid_size
        end if

        if (size(xout) /= dst_grid_size) then
           print *, "Interpolation warning: output grid size does not match:"
           print *, "   Output (xout) size   = ", size(xout)
           print *, "   Expected size        = ", dst_grid_size
        end if

        xout = 0.0

! FIXME: normalization?
        if ((present(src_grad_lat)) .and. (num_wgts == 3)) then
! FIXME: check gradients for size consistency?
           do i=1,num_links
             xout(dst_address(i)) = xout(dst_address(i)) +             &
      		    remap_matrix(1,i)*xin(src_address(i)) +            &
      		    remap_matrix(2,i)*src_grad_lat(src_address(i)) +   &
      		    remap_matrix(3,i)*src_grad_lon(src_address(i))

!          print *, "xin (",                                            &
!                   src_address(i), ") = ",                             &
!                   xin(src_address(i)), "   =>   xout (",              &
!                   dst_address(i), ") = ",                             &
!                   xout(dst_address(i))

           end do
        else
           do i=1,num_links
             xout(dst_address(i)) = xout(dst_address(i)) +             &
      				    remap_matrix(1,i)*xin(src_address(i))

!          print *, "xin (",                                            &
!                   src_address(i), ") = ",                             &
!                   xin(src_address(i)), "   =>   xout (",              &
!                   dst_address(i), ") = ",                             &
!                   xout(dst_address(i))

           end do
        end if


	END SUBROUTINE interp4




!-----------------------------------------------------------------------
!
	SUBROUTINE interp8(xin, xout, src_grad_lat, src_grad_lon)
!
!-----------------------------------------------------------------------
!  Purpose:
!   Interpolates real*8 input data (xin) 
!
!  Scope: private (with public module interface)
!    Module variables used:
!       src_grid_size
!       dst_grid_size
!       num_links
!       num_wgts
!       dst_address
!       src_address
!       remap_matrix
!    Arguments:
!       xin: input data (defined on source grid)
!       xout: output data (defined on destination grid)
!       src_grad_lat: gradient of input data (on source grid)
!       src_grad_lon: gradient of input data (on source grid)
!
! Bugs:
!    1) cannot be called before interp_init
!
!-----------------------------------------------------------------------

	real*8, dimension(:), intent(in) :: xin
	real*8, dimension(:), intent(out) :: xout

	real*8, dimension(:), intent(in), optional :: src_grad_lat
	real*8, dimension(:), intent(in), optional :: src_grad_lon

        integer :: i

! check arrays for size consistency
        if (size(xin) /= src_grid_size) then
           print *, "Interpolation warning: input grid size does not match:"
           print *, "   Input (xin) size  = ", size(xin)
           print *, "   Expected size     = ", src_grid_size
        end if

        if (size(xout) /= dst_grid_size) then
           print *, "Interpolation warning: output grid size does not match:"
           print *, "   Output (xout) size  = ", size(xout)
           print *, "   Expected size       = ", dst_grid_size
        end if

        xout = 0.0

! FIXME: normalization?
        if ((present(src_grad_lat)) .and. (num_wgts == 3)) then
! FIXME: check gradients for size consistency?
           do i=1,num_links
             xout(dst_address(i)) = xout(dst_address(i)) +             &
      		    remap_matrix(1,i)*xin(src_address(i)) +            &
      		    remap_matrix(2,i)*src_grad_lat(src_address(i)) +   &
      		    remap_matrix(3,i)*src_grad_lon(src_address(i))

!          print *, "xin (",                                            &
!                   src_address(i), ") = ",                             &
!                   xin(src_address(i)), "   =>   xout (",              &
!                   dst_address(i), ") = ",                             &
!                   xout(dst_address(i))

           end do
        else
           do i=1,num_links
             xout(dst_address(i)) = xout(dst_address(i)) +             &
      				    remap_matrix(1,i)*xin(src_address(i))

!          print *, "xin (",                                            &
!                   src_address(i), ") = ",                             &
!                   xin(src_address(i)), "   =>   xout (",              &
!                   dst_address(i), ") = ",                             &
!                   xout(dst_address(i))

           end do
        end if
 

	END SUBROUTINE interp8


!-----------------------------------------------------------------------
!
	SUBROUTINE interp4_out(outfilename, varname, xout)
!
!-----------------------------------------------------------------------
! Purpose:
!   Outputs the given data to a netcdf file 
!   Takes real*4 data variable xout
!
! Scope: private (with public module interface) 
!   Module variables set:
!       first (changed if true)
!   Module routines used:
!       get_dst_grid_info
!       define_out_grid_info
!       write_out_grid_info
!   Internal routines:
!       define4_xout
!       write4_xout
!   Arguments:
!       outfilename - name of the netcdf output file
!       varname - name of the variable
!       xout - real*4 data 
!
! Bugs:
!   1) The definition routines and writing routines are very tightly
!      coupled - they must be called in the following order:
!       i)   get_dst_grid_info 
!       ii)  define_out_grid_info
!       iii) define_xout 
!       iv)  write_xout
!       v)   write_out_grid_info
!       
!-----------------------------------------------------------------------

        character(len=*), intent(in) :: outfilename
        character(len=*), intent(in) :: varname
        real*4, dimension(:), intent(in) :: xout


        integer :: status, outfileID
        integer :: xoutid

! define grid data (only if this is the first call)
        if (first) then

! FIXME: change back to NF_NOCLOBBER
           status = nf_create (outfilename, NF_CLOBBER, outfileID)
           call handle_error (status, outfilename)

           call get_dst_grid_info
           call define_out_grid_info (outfileID)
           call define4_xout ()

           status = nf_enddef(outfileID)
           call handle_error (status, outfilename)

           call write4_xout()
           call write_out_grid_info (outfileID)

           status = nf_close (outfileID)
	   call handle_error (status)

           first = .false.

        else   ! not the first variable written to this file

           status = nf_open (outfilename, NF_WRITE, outfileID)
           call handle_error (status, outfilename)

           status = nf_redef (outfileID)
           call handle_error (status, outfilename)

           call define4_xout()

           status = nf_enddef (outfileID)
           call handle_error (status, outfilename)

           call write4_xout()

           status = nf_close (outfileID)
	   call handle_error (status)

        end if


	CONTAINS


!-----------------------------------------------------------------------
!
	SUBROUTINE define4_xout ()
!
!-----------------------------------------------------------------------
!  Purpose:
!     Define the (interpolated) variable to be output
! 
!  Scope: private, contained in subroutine interp8_out
!    Module variables used:
!       dst_grid_rank
!       grid_dim_ids
!    Module routines called:
!    Containing subroutine (interp4_out) variables used:
!       status
!       outfileID
!       varname
!       xoutid (set)
! 
!  Bugs:
!    1) The output grid must be defined (by calling subroutine 
!       "define_out_grid_info") before this routine is called.
!       This is because the dimension ids are used here.
!-----------------------------------------------------------------------

!
! define the variable 
!   output variable should be dimensioned in the same manner as
!   the output grid (dst_grid_rank dimensions, dimension i has
!   length dst_grid_dims(i).)
!
        status = nf_def_var (outfileID, varname, NF_REAL,              &
                             dst_grid_rank, grid_dim_ids, xoutid)
        call handle_error (status, varname)

! CUSTOMIZE: add attributes for output variables here

	END SUBROUTINE define4_xout



!-----------------------------------------------------------------------
!
	SUBROUTINE write4_xout ()
!
!-----------------------------------------------------------------------
!  Purpose:
!     Write the (interpolated) variable to netcdf file
! 
!  Scope: private, contained in subroutine interp4_out
!    Module routines called:
!    Containing subroutine (interp4_out) variables used:
!       status
!       outfileID
!       xoutid
!       varname
!       xout
! 
!-----------------------------------------------------------------------

! write the data

        status = nf_put_var_real (outfileID, xoutid, xout)
        call handle_error (status, varname)


	END SUBROUTINE write4_xout


	END SUBROUTINE interp4_out




!-----------------------------------------------------------------------
!
	SUBROUTINE interp8_out(outfilename, varname, xout)
!
!-----------------------------------------------------------------------
! Purpose:
!   Outputs the given data to a netcdf file 
!   Takes real*8 data variable xout
!
! Scope: private (with public module interface) 
!   Module variables set:
!       first (changed if true)
!   Module routines used:
!       get_dst_grid_info
!       define_out_grid_info
!       write_out_grid_info
!   Internal routines:
!       define8_xout
!       write8_xout
!   Arguments:
!       outfilename - name of the netcdf output file
!       varname - name of the variable
!       xout - real*8 data 
!
! Bugs:
!   1) The definition routines and writing routines are very tightly
!      coupled - they must be called in the following order:
!       i)   get_dst_grid_info 
!       ii)  define_out_grid_info
!       iii) define_xout 
!       iv)  write_xout
!       v)   write_out_grid_info
!       
!-----------------------------------------------------------------------

        character(len=*), intent(in) :: outfilename
        character(len=*), intent(in) :: varname
        real*8, dimension(:), intent(in) :: xout

        integer :: status, outfileID
	integer :: varid


        if (first) then   ! create output file

! FIXME: change back to NF_NOCLOBBER
           status = nf_create (outfilename, NF_CLOBBER, outfileID)
           call handle_error (status, "File: "//outfilename)

           call get_dst_grid_info
           call define_out_grid_info (outfileID)
           call define8_xout ()

           status = nf_enddef(outfileID)
           call handle_error (status, outfilename)

           call write8_xout()
           call write_out_grid_info (outfileID)

           status = nf_close(outfileID)
	   call handle_error (status)

           first = .false.

        else   ! not the first variable written to this file

           status = nf_open (outfilename, NF_WRITE, outfileID)
	   call handle_error (status)

           status = nf_redef (outfileID)
	   call handle_error (status)

           call define8_xout()

           status = nf_enddef (outfileID)
	   call handle_error (status)

           call write8_xout()

           status = nf_close (outfileID)
	   call handle_error (status)

        end if



	CONTAINS


!-----------------------------------------------------------------------
!
	SUBROUTINE define8_xout ()
!
!-----------------------------------------------------------------------
!  Purpose:
!     Define the (interpolated) variable to be output
! 
!  Scope: private, contained in subroutine interp8_out
!    Module variables used:
!       dst_grid_rank
!       grid_dim_ids
!    Module routines called:
!    Containing subroutine (interp8_out) variables used:
!       status
!       outfileID
!       varname
!       varid (set)
! 
!-----------------------------------------------------------------------


! define the variable 
        status = nf_def_var (outfileID, varname, NF_DOUBLE,            &
                             dst_grid_rank, grid_dim_ids, varid)
        call handle_error (status, varname)

! CUSTOMIZE: add attributes common to all output real*8 variables here

	END SUBROUTINE define8_xout



!-----------------------------------------------------------------------
!
	SUBROUTINE write8_xout ()
!
!-----------------------------------------------------------------------
!  Purpose:
!     Write the (interpolated) variable to netcdf file
! 
!  Scope: private, contained in subroutine interp8_out
!    Module routines called:
!    Containing subroutine (interp8_out) variables used:
!       status
!       outfileID
!       varid
!       varname
!       xout
! 
!-----------------------------------------------------------------------

! write the data
        status = nf_put_var_double (outfileID, varid, xout)
        call handle_error (status, varname)


	END SUBROUTINE write8_xout


	END SUBROUTINE interp8_out




!-----------------------------------------------------------------------
!
	SUBROUTINE get_dst_grid_info ()
!
!-----------------------------------------------------------------------
!  Purpose:
!     Gets information about the destination grid (to be written later.)
!     Breaks output into more manageable pieces.
!
!  Scope:  private 
!   Module variables used:
!       remapfile
!       netcdf stuff
!       dst_grid_size
!   Module variables set:
!       dst_grid_rank
!       dst_grid_corners
!       dst_grid_dims
!       dst_grid_center_lat
!       dst_grid_center_lon
!       dst_grid_corner_lat
!       dst_grid_corner_lon
!       dst_grid_imask
!   Module routines used:
!  
!  Bugs:
!     1) This subroutine allocates arrays that are not deallocated
!        within its scope (e.g., dst_grid_center_lat, dst_grid_center_lon.) 
!        These are deallocated by write_out_grid_info.
!        note: grid_dim_ids is deallocated by interp_done, since it
!              is needed to define output variables (more than once...)
!-----------------------------------------------------------------------

! temporary - to hold variable ids
        integer :: ncid

        integer :: fileID, status


! re-open remapping file
        status = nf_open (remapfile, NF_NOWRITE, fileID)
	call handle_error (status, "file = "//remapfile)

! get destination grid sizes
        status = nf_inq_dimid (fileID, "dst_grid_rank", ncid)
	call handle_error (status, "dst_grid_rank")
	status = nf_inq_dimlen (fileID, ncid, dst_grid_rank)
	call handle_error (status, "dst_grid_rank")

        status = nf_inq_dimid (fileID, "dst_grid_corners", ncid)
	call handle_error (status, "dst_grid_corners")
	status = nf_inq_dimlen (fileID, ncid, dst_grid_corners)
	call handle_error (status, "dst_grid_corners")


        allocate (grid_dim_ids(dst_grid_rank))
        allocate (dst_grid_dims(dst_grid_rank))
        allocate (dst_grid_imask(dst_grid_size))
	allocate (dst_grid_center_lat(dst_grid_size))
	allocate (dst_grid_center_lon(dst_grid_size))
	allocate (dst_grid_corner_lat(dst_grid_corners, dst_grid_size))
	allocate (dst_grid_corner_lon(dst_grid_corners, dst_grid_size))


! get destination grid coordinate info (to be written to output grid)
	status = nf_inq_varid (fileID, "dst_grid_dims", ncid)
	call handle_error (status, "dst_grid_dims")
	status = nf_get_var_int (fileID, ncid, dst_grid_dims)
	call handle_error (status, "dst_grid_dims")

	status = nf_inq_varid (fileID, "dst_grid_imask", ncid)
	call handle_error (status, "dst_grid_imask")
	status = nf_get_var_int (fileID, ncid, dst_grid_imask)
	call handle_error (status, "dst_grid_imask")

	status = nf_inq_varid (fileID, "dst_grid_center_lat", ncid)
	call handle_error (status, "dst_grid_center_lat")
	status = nf_get_var_double (fileID, ncid, dst_grid_center_lat)
	call handle_error (status, "dst_grid_center_lat")

	status = nf_inq_varid (fileID, "dst_grid_center_lon", ncid)
	call handle_error (status, "dst_grid_center_lon")
	status = nf_get_var_double (fileID, ncid, dst_grid_center_lon)
	call handle_error (status, "dst_grid_center_lon")

	status = nf_inq_varid (fileID, "dst_grid_corner_lat", ncid)
	call handle_error (status, "dst_grid_corner_lat")
	status = nf_get_var_double (fileID, ncid, dst_grid_corner_lat)
	call handle_error (status, "dst_grid_corner_lat")

	status = nf_inq_varid (fileID, "dst_grid_corner_lon", ncid)
	call handle_error (status, "dst_grid_corner_lon")
	status = nf_get_var_double (fileID, ncid, dst_grid_corner_lon)
	call handle_error (status, "dst_grid_corner_lon")

! done with remap file 
	status = nf_close (fileID)
	call handle_error (status)

	END SUBROUTINE get_dst_grid_info




!-----------------------------------------------------------------------
!
        SUBROUTINE define_out_grid_info (fileID)
!
!-----------------------------------------------------------------------
!  Purpose:
!    Define destination grid variables
!    This is separate from writing so that file access is more efficient
!    the first time a variable is written.
!
!  Scope: private
!    Module variables used:
!       dst_grid_size
!       dst_grid_rank
!       dst_grid_corners
!    Module variables set:
!       out_sizeID
!       out_rankID
!       out_cornerID
!       grid_dims_id
!       grid_imask_id
!       grid_center_lat_id
!       grid_center_lon_id
!       grid_corner_lon_id
!       grid_corner_lat_id
!
!-----------------------------------------------------------------------

	integer, intent(in) :: fileID

	integer :: status, n

 	integer, dimension(1) :: tmparray
 	integer, dimension(:), allocatable :: dimarray

        character(len=1) :: nchar

! define necessary dimensions
        status = nf_def_dim(fileID,"grid_size",dst_grid_size,out_sizeID)
        call handle_error (status, "grid_size")

        status = nf_def_dim(fileID,"grid_rank",dst_grid_rank,out_rankID)
        call handle_error (status, "grid_rank")

        status = nf_def_dim(fileID, "grid_corners", dst_grid_corners,  &
                            out_cornerID)
        call handle_error (status, "grid_corners")

! define a variable number of output grid dimensions - 
! grid_dims_1 ... grid_dims_n, where n = dst_grid_rank.
        do n = 1, dst_grid_rank
           write (unit=nchar, fmt='(I1)') n
           status = nf_def_dim(fileID, "grid_dim_"//nchar,             &
                               dst_grid_dims(n), grid_dim_ids(n))
           call handle_error (status, "grid_dim"//nchar)
        end do

! define variables 
        tmparray = (/out_rankID/)
        status = nf_def_var(fileID, "grid_dims", NF_INT, 1, tmparray,  &
                            grid_dims_id)

        status = nf_def_var(fileID, "grid_center_lat", NF_DOUBLE,      &
                            dst_grid_rank, grid_dim_ids, grid_center_lat_id)
        status = nf_def_var(fileID, "grid_center_lon", NF_DOUBLE,      &
                            dst_grid_rank, grid_dim_ids, grid_center_lon_id)
        status = nf_def_var(fileID, "grid_imask", NF_INT,              &
                            dst_grid_rank, grid_dim_ids, grid_imask_id)

! store corners on destination grid
        allocate (dimarray(dst_grid_rank+1))
        dimarray(1) = out_cornerID
        dimarray(2:) = grid_dim_ids
        status = nf_def_var(fileID, "grid_corner_lat", NF_DOUBLE,      &
                            dst_grid_rank+1, dimarray, grid_corner_lat_id)
        status = nf_def_var(fileID, "grid_corner_lon", NF_DOUBLE,      &
                            dst_grid_rank+1, dimarray, grid_corner_lon_id)
        deallocate (dimarray)


	END SUBROUTINE define_out_grid_info




!-----------------------------------------------------------------------
!
        SUBROUTINE write_out_grid_info (fileID)
!
!-----------------------------------------------------------------------
!  Purpose:
!    Output grid information for destination grid
!
!  Scope: private
!    Module variables used:
!       grid_dim_ids
!       grid_dims_id
!       grid_imask_id
!       grid_center_lat_id
!       grid_center_lon_id
!       grid_corner_lon_id
!       grid_corner_lat_id
!       dst_grid_center_lat
!       dst_grid_center_lon
!       dst_grid_corner_lat
!       dst_grid_corner_lon
!    Module routines used:
!       handle_error
!       netcdf stuff
!    
!  Bugs:
!    1) module variables must be allocated and given values before
!       this routine is called.
!    2) deallocate variables that are not allocated here.
!       (grid routines must be called in sequence)
!
!-----------------------------------------------------------------------

        integer, intent(in) :: fileID

 	integer :: status


! write data
        status = nf_put_var_int(fileID, grid_dims_id, dst_grid_dims)
        call handle_error (status, "grid_dims")

        status = nf_put_var_double(fileID, grid_center_lat_id, dst_grid_center_lat)
        call handle_error (status, "grid_center_lat")

        status = nf_put_var_double(fileID, grid_center_lon_id, dst_grid_center_lon)
        call handle_error (status, "grid_center_lon")

        status = nf_put_var_int(fileID, grid_imask_id, dst_grid_imask)
        call handle_error (status, "grid_imask")

        status = nf_put_var_double(fileID, grid_corner_lat_id, dst_grid_corner_lat)
        call handle_error (status, "grid_corner_lat")


        status = nf_put_var_double(fileID, grid_corner_lon_id, dst_grid_corner_lon)
        call handle_error (status, "grid_corner_lon")



! deallocate arrays allocated within get_dst_grid_info
	deallocate (dst_grid_dims)
	deallocate (dst_grid_imask)
	deallocate (dst_grid_center_lat)
	deallocate (dst_grid_center_lon)
	deallocate (dst_grid_corner_lat)
	deallocate (dst_grid_corner_lon)


	END SUBROUTINE write_out_grid_info




!-----------------------------------------------------------------------
!
        SUBROUTINE interp_done ()
!
!-----------------------------------------------------------------------
!  Purpose:
!    Deallocates module data allocated in interp_init
! 
!  Scope: public
!    Module variables used:
!       src_address
!       dst_address
!       remap_matrix
!-----------------------------------------------------------------------

        if (allocated(grid_dim_ids)) deallocate(grid_dim_ids)

        if (allocated(src_address)) then
      	   deallocate (src_address)
	   deallocate (dst_address)
	   deallocate (remap_matrix)
        end if


	END SUBROUTINE interp_done


	END MODULE interpolate

