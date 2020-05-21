  PROGRAM driver

!----------------------------------------------------------------------
! 7-13-99 written by the CSU General Circulation Modeling Group
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! VARIABLE DEFINITION
!
!  remapfile  : netCDF file containing the remapping weights
!  src_size  : number of grid cells on the grid which the data
!        is interpolated FROM, set dynamically
!  dst_size  : number of grid cells on the grid which the data
!                   is interpolated TO, set dynamically
!----------------------------------------------------------------------

        use interpolate
        use getdata
        use netcdf
        implicit none
        INCLUDE "lib3f.h"


! define the filename for the namelist file
        character(255) :: nmlistfn

! define input, output, error units
        integer, parameter :: stdin  = 5 ! reserves unit for standard input
        integer, parameter :: stdout  = 6 ! reserves unit for standard output
        integer, parameter :: stderr  = 6 ! reserves unit for standard error

! define the vars to be read from the namelist file
!   remapfile contains the data which allows interplolation between grids
!   outfile defines the new netCDF output file
!   datafile defines the existing netCDF input file
!   varname is the variable to interpolate
!   level is the name of the level to process (e.g. 700, 850)
        character(len=255) :: remapfile, outfile, datafile
        character(len=30) :: varname
        integer :: level, lvl_id
        namelist /interpolate_in/ remapfile, outfile, datafile, varname, level

! netCDF related vars
        integer :: ncid_data, ncid_rmp, ncid_out, ncid, rcstat
        integer :: nDims, nVars, nGlobalAtts, unlimDimID
        integer :: nDims_rm, nVars_rm, nGlobalAtts_rm, unlimDimID_rm
        integer :: alen, attr_length, attr_length2, xtype, xtype2
        integer :: nTimes, nLvls, lvl_ix
        integer, dimension(:), allocatable :: avail_lvls
        character(255) :: attname, new_attname, new_attvalue, ivarname

! define remaining local vars
        real*4, dimension(:,:,:), allocatable :: xin4
        real*4, dimension(:,:), allocatable :: xout4
        real*4, dimension(:,:), allocatable :: xerr4
        character(255) :: history, rmpdate, rmptime, rmpzone

        integer :: src_size, dst_size
        integer :: ierr, nargs
        integer :: i, j, k

!
! get namelist filename from command line, or default to "interpolate_in"
!
        nargs = iargc()
        if (nargs .lt. 1) then
          nmlistfn='interpolate_in'
        else
          call getarg(1,nmlistfn)
        end if

!
! use the namelist to define the names of the input and output
! grid files, the interpolation search radius (guessp) and the
! input/output grid dimensions
!
        level = 3  ! default value (700 mb)
        write (stdout,*) ' Using namelist file ',trim(nmlistfn)
        open(unit=15,file=nmlistfn,status='old',iostat=ierr)
        if (ierr .NE. 0) then
          write(*,111) ierr
 111      format('readfile: Error: IOSTAT ',i5,' opening namelist interpolate.in')
          goto 9999
        endif
        read(15,nml=interpolate_in)
        close(unit=15)

!
! create output file
!
        write(stdout, *) ' Output = ', outfile(1:len_trim(outfile))
        rcstat = nf_create( outfile, NF_CLOBBER, ncid_out)
        if (rcstat /= nf_noerr) call handle_error( rcstat, "Create output file")
!
! add input and remap files and current time as global attributes
!
        attr_length = len_trim( datafile )
        rcstat = nf_put_att_text(ncid_out, NF_GLOBAL, "Input_file", attr_length, trim(datafile))
        if (rcstat /= nf_noerr) call handle_error( rcstat, "Create global attrib")
        attr_length = len_trim( remapfile )
        rcstat = nf_put_att_text(ncid_out, NF_GLOBAL, "Remap_file", attr_length, trim(remapfile))
        if (rcstat /= nf_noerr) call handle_error( rcstat, "Create global attrib")

        call date_and_time(date=rmpdate, time=rmptime, zone=rmpzone)
        write (history,1000) rmpdate(5:6),rmpdate(7:8),rmpdate(1:4),rmptime(1:2),rmptime(3:4),rmptime(5:6),trim(rmpzone)
   1000 format(a2,'-',a2,'-',a4,' ',a2,':',a2,':',a2,' ',a)
        attr_length = len_trim( history )
        rcstat = nf_put_att_text(ncid_out, NF_GLOBAL, "Remapping_date", attr_length, trim(history))
        if (rcstat /= nf_noerr) call handle_error( rcstat, "Create global attrib")
!
! open input file
!
        write(stdout, *) ' Input = ', datafile(1:len_trim(datafile))
        rcstat = nf_open( datafile, nf_nowrite, ncid_data )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "bad data file")

!
! save original dimensions as global attributes
!
        rcstat = nf_inq_dimid( ncid_data, "lat", ncid )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "lat dim not found")
        rcstat = nf_inq_dim( ncid_data, ncid, ivarname, attr_length )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "lat dim inq failed")
        rcstat = nf_put_att_int( ncid_out, NF_GLOBAL, "Original_lat", nf_int, 1, attr_length )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "Create global attrib")

        rcstat = nf_inq_dimid( ncid_data, "lon", ncid )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "lon dim not found")
        rcstat = nf_inq_dim( ncid_data, ncid, ivarname, attr_length )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "lon dim inq failed")
        rcstat = nf_put_att_int( ncid_out, NF_GLOBAL, "Original_lon", nf_int, 1, attr_length )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "Create global attrib")

!
! copy global attributes from input file
!
        rcstat = nf_inq( ncid_data, nDims, nVars, nGlobalAtts, unlimDimID )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "Inquire failed")
        write(stdout, 1050) nDims, nVars, nGlobalAtts
  1050  format('  Input file characteristics:', i2, ' Dimensions,', i2, ' Variables,', i2, ' Global Attributes')

        if (nDims .gt. 3) then  ! it's a timexlevelxlatxlon dataset
          rcstat = nf_inq_dimid( ncid_data, "level", ncid )
          if (rcstat /= nf_noerr) call handle_error( rcstat, "level dim not found")
          rcstat = nf_inq_dim( ncid_data, ncid, ivarname, nLvls )
          if (rcstat /= nf_noerr) call handle_error( rcstat, "level dim inq failed")
          allocate( avail_lvls( nLvls ) )
          rcstat = nf_inq_varid( ncid_data, "level", lvl_id )
          if (rcstat /= nf_noerr) call handle_error( rcstat, "get id levels failed")
          rcstat = nf_get_var_int( ncid_data, lvl_id, avail_lvls )
          if (rcstat /= nf_noerr) call handle_error( rcstat, "read levels failed")

! find the desired level in the available levels
          do i=1,nLvls
            if (avail_lvls(i) .eq. level) then
              lvl_ix = i
              goto 300
            end if
          end do
! exact match not found, finding closest
          write(stdout,*) ' Exact level match not found, finding closest'
          do i=1,nLvls-1
            write(stdout,*) ' Checking ',avail_lvls(i),' and ',avail_lvls(i+1)
            if (avail_lvls(i) .gt. level .and. avail_lvls(i+1) .lt. level) then
              j = avail_lvls(i) - level
              k = level - avail_lvls(i+1)
              if (j .le. k) then
                lvl_ix = i
              else
                lvl_ix = i+1
              end if
              goto 250
            end if
          end do
 250      continue
          write(stdout,*) ' Level requested=',level,' Closest level=',avail_lvls(lvl_ix)
 300      continue
        else  ! it's a timexlatxlon dataset, i.e., just one level to process
          nLvls = 1
          lvl_ix = 1
        end if

!        write(stdout, *) ' Copying input file global attributes'
        do i=1,nGlobalAtts
          rcstat = nf_inq_attname( ncid_data, NF_GLOBAL, i, attname )
          if (rcstat /= nf_noerr) call handle_error( rcstat, "Inquiry failed")
          alen = len_trim(attname)
          rcstat = nf_inq_att( ncid_data, NF_GLOBAL, attname(1:alen), xtype, attr_length )
          if (rcstat /= nf_noerr) call handle_error( rcstat, "Inquiry attr failed")
!          write(stdout, *) ' ',attname(1:alen)
          rcstat = nf_copy_att( ncid_data, NF_GLOBAL, attname(1:alen), ncid_out, NF_GLOBAL )
          if (rcstat /= nf_noerr) call handle_error( rcstat, "Copy attr failed")
        end do
!
! copy global attributes from remap file
!
        rcstat = nf_open( remapfile, nf_nowrite, ncid_rmp )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "bad remap file")
        rcstat = nf_inq( ncid_rmp, nDims_rm, nVars_rm, nGlobalAtts_rm, unlimDimID_rm )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "Inquire failed")
        write(stdout, *) ' Remap = ', remapfile(1:len_trim(remapfile))
        write(stdout, 1055) nDims_rm, nVars_rm, nGlobalAtts_rm
  1055  format('  Remap file characteristics: ', i2, ' Dimensions, ', i2, ' Variables, ', i2, ' Global Attributes')

!        write(stdout, *) ' Copying remap file global attributes'
        do i=1,nGlobalAtts_rm
          rcstat = nf_inq_attname( ncid_rmp, NF_GLOBAL, i, attname )
          if (rcstat /= nf_noerr) call handle_error( rcstat, "Inquiry failed")
          alen = len_trim(attname)
          rcstat = nf_inq_att( ncid_rmp, NF_GLOBAL, attname(1:alen), xtype, attr_length )
          if (rcstat /= nf_noerr) call handle_error( rcstat, "Inquiry attr failed")
!          write(stdout, *) ' ',attname(1:alen)

          ! check for attribute already in output file
          ! if it's there, copy from remap and rename then recopy from data
          rcstat = nf_inq_att( ncid_out, NF_GLOBAL, attname(1:alen), xtype2, attr_length2 )
          if (rcstat == nf_noerr) then
            write(stdout, *) ' Attribute ',attname(1:alen),' already exists in output, renaming'
            rcstat = nf_copy_att( ncid_rmp, NF_GLOBAL, attname(1:alen), ncid_out, NF_GLOBAL )
            if (rcstat /= nf_noerr) call handle_error( rcstat, "Copy attr failed")
            new_attname = attname(1:alen) // "_rmp"
            rcstat = nf_rename_att( ncid_out, NF_GLOBAL, attname(1:alen), new_attname )
            if (rcstat /= nf_noerr) call handle_error( rcstat, "Rename attr failed")
            rcstat = nf_copy_att( ncid_data, NF_GLOBAL, attname(1:alen), ncid_out, NF_GLOBAL )
            if (rcstat /= nf_noerr) call handle_error( rcstat, "Copy attr failed")
          else ! otherwise just copy from remap
            rcstat = nf_copy_att( ncid_rmp, NF_GLOBAL, attname(1:alen), ncid_out, NF_GLOBAL )
            if (rcstat /= nf_noerr) call handle_error( rcstat, "Copy attr failed")
          end if
        end do

! close the remap file
        rcstat = nf_close( ncid_rmp )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "Close input failed")

! close the output file so that further processing can proceed
        rcstat = nf_close( ncid_out )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "Close output failed")

! get info on the time dimension, if present
        if (nDims .ge. 3) then
          rcstat = nf_inq_dimid (ncid_data, "time", ncid)
          if (rcstat /= nf_noerr) call handle_error( rcstat, "Time dimension query")
          rcstat = nf_inq_dimlen (ncid_data, ncid, nTimes)
          if (rcstat /= nf_noerr) call handle_error( rcstat, "Time dimension get")
!          write(stdout, 1060) nTimes
!  1060    format('  Input has ', i4, ' time steps')
        else
          nTimes = 1
        end if

! close the input data file
        rcstat = nf_close( ncid_data )
        if (rcstat /= nf_noerr) call handle_error( rcstat, "Close input failed")

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
        call interp_init(remapfile, src_size, dst_size, datafile(1:len_trim(datafile)))
        allocate (xin4(nTimes,nLvls,src_size), xout4(nTimes,dst_size))

!----------------------------------------------------------------------
! call the read routine
!----------------------------------------------------------------------
        call readdata( datafile, varname, xin4)

!----------------------------------------------------------------------
! do the grid interpolation
!----------------------------------------------------------------------
        write(stdout,*) ' Starting interpolation loop'
        do i=1,nTimes
          if (modulo(i,90) .eq. 0) write(stdout,1500) i
   1500   format('  Time step ',i4)
          call interp(xin4(i,lvl_ix,:), xout4(i,:))
        end do

!----------------------------------------------------------------------
! write the new data to the output file
!----------------------------------------------------------------------
        call interp_out(outfile, varname, xout4)

!----------------------------------------------------------------------
! remember to finish with a call to interp_done, so that allocated 
! memory is freed properly.
!----------------------------------------------------------------------
        call interp_done()

9999    continue
        END PROGRAM driver
