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
        real*4, dimension(:,:,:), intent(out) :: data
        real*4, dimension(:), allocatable :: temp_data
        integer, dimension(3) :: data_shape

        integer :: fileID, varID, status, nTimes, nLvls, nPoints, nPtsLvls
        integer :: i, j, start_ptr, end_ptr

        data_shape = shape( data )
        nTimes = data_shape(1)
        nLvls = data_shape(2)
        nPoints = data_shape(3)
        nPtsLvls = nPoints*nLvls
        write(6, 1050) nTimes, nLvls, nPoints
  1050  format('  Input file: ', i3, ' Time(s),', i3, ' Level(s),' i6, ' Points')
        allocate( temp_data( nTimes*nLvls*nPoints ))

        status = nf_open (filename, NF_NOWRITE, fileID)
        call handle_error (status, filename)

        status = nf_inq_varid (fileID, varname, varID)
        call handle_error (status, varname)

        write(6,*) ' Reading input file data'
        status = nf_get_var_real(fileID, varID, temp_data)
        call handle_error (status, varname)

        do i=1,nTimes
          do j=1,nLvls
            start_ptr = (nPoints * (j-1)) + (nPtsLvls * (i-1)) + 1
            end_ptr = start_ptr + nPoints -1
            data(i,j,:) = temp_data(start_ptr:end_ptr)
          end do
        end do

        status = nf_close (fileID)
        call handle_error (status, filename)
        deallocate( temp_data )


  END SUBROUTINE readdata

  END MODULE getdata
