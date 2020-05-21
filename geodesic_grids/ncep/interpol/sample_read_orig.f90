!-----------------------------------------------------------------------
!
	MODULE getdata
!
!-----------------------------------------------------------------------
!  Purpose:
!    Contains example routines for getting data
!
!-----------------------------------------------------------------------

        use netcdf

	implicit none
	private
        
				public :: readdata


	CONTAINS


!-----------------------------------------------------------------------
!
	SUBROUTINE readdata(filename, varname, data)
!
!-----------------------------------------------------------------------
!  Purpose:
!    Example read routine to get data from a netcdf file
!
!-----------------------------------------------------------------------

        character (len=*), intent(in) :: filename, varname
	real*8, dimension(:), intent(out) :: data


        integer :: fileID, varID, status

	status = nf_open (filename, NF_NOWRITE, fileID)
	call handle_error (status, filename)

        status = nf_inq_varid (fileID, varname, varID)
        call handle_error (status, varname)

        status = nf_get_var_double (fileID, varID, data)
        call handle_error (status, varname)

        status = nf_close (fileID)
        call handle_error (status, filename)


	END SUBROUTINE readdata

	END MODULE getdata
