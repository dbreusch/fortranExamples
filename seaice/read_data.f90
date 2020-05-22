! read_data.f
! Convert NSIDC binary sea-ice files at d1-00h and d2-0h into four
! WPS intermediate files at d1-00h, d1-06h, d1-12h, d1-18h.
! Compile with "ifort -o read_data read_data.f"
!
! Big-endian vs little-endian I/O:
! - the sea-ice input file (UNIT 20) is little-endian; this requires
!   "setenv XLFRTEOPTS ufmt_littleendian=20" when running on a big-endian
!   system (e.g., Janus) and using xlf90.
! - the sea-ice output file (UNIT 11) is big-endian; this requires
!   "setenv FORT_CONVERT11 BIG_ENDIAN" when running on a little-endian
!   system (e.g, Yellowstone, Linux) and using ifort.

PROGRAM read_data

  IMPLICIT none

  integer i, j, k, nelem, fnhh, len_i1, len_i2, len_o
  integer :: iunit = 20, ounit = 11, nlunit = 21
  character (len=256) :: ifn1     ! input file
  character (len=256) :: ifn2     ! input file 2
  character (len=256) :: ofn      ! output file
  character (len=256) :: dynfmt   ! dynamic format string

  integer :: version = 5         ! Format version (must =5 for WPS format)
  integer :: iproj = 5           ! Code for projection of data in array:
                                 !       0 = cylindrical equidistant
                                 !       1 = Mercator
                                 !       3 = Lambert conformal conic
                                 !       4 = Gaussian (global only!)
                                 !       5 = Polar stereographic
  real :: nlats                  ! Number of latitudes north of equator
                                 !       (for Gaussian grids)
  real :: xfcst = 0              ! Forecast hour of data
  real :: xlvl = 200100.         ! Vertical level of data in 2-d array
  real :: deltalat, deltalon     ! Grid spacing, degrees
  real :: dx = 25., dy = 25.     ! Grid spacing, km
  real :: truelat2               ! True latitudes of projection
  real :: earth_radius           ! Earth radius, km

  ! 2-d arrays holding the data
  integer(2), allocatable :: idata(:)
  real, allocatable :: slab00(:,:), slab06(:,:), slab12(:,:)
  real, allocatable :: slab18(:,:), slab24(:,:)

  logical :: is_wind_grid_rel    ! Flag indicating whether winds are
                                 !       relative to source grid (TRUE) or
                                 !       relative to earth (FALSE)
  character (len=9)  :: field    ! Name of the field
  character (len=24) :: hdate    ! Valid date for data YYYY:MM:DD_HH:00:00
  character (len=25) :: units    ! Units of data
  character (len=32) :: map_source  !  Source model / originating center
  character (len=46) :: desc     ! Short description of data

  ! define namelist vars
  ! 1) hemisphere-dependent parameters
  character (len=1) :: hemi      ! which hemisphere
  integer ni, nj                 ! x- and y-dimensions of 2-d array
  real :: truelat1               ! True latitudes of projection
  real :: xlonc                  ! Standard longitude of projection
  real :: startlat, startlon     ! Lat/lon of point in array indicated by
                                 !       startloc string
  character (len=8)  :: startloc ! Which point in array is given by
                                 !       startlat/startlon; set either
                                 !       to 'SWCORNER' or 'CENTER  '
  namelist /rundata/ hemi, ni, nj, truelat1, xlonc, startlat, startlon, startloc

  ! 2a) today's file
  character (len=4) :: fnyy      ! input file year
  character (len=2) :: fnmm      ! input file month
  character (len=2) :: fndd      ! input file day
  character (len=4) :: fnsensor  ! input file sensor string (e.g., f08)
  character (len=4) :: fnversion ! input file version string (e.g., v03)
  ! 2b) next day's file
  character (len=4) :: ndyy      ! input file year
  character (len=2) :: ndmm      ! input file month
  character (len=2) :: nddd      ! input file day
  character (len=4) :: ndsensor  ! input file sensor string (e.g., f08)
  character (len=4) :: ndversion ! input file version string (e.g., v03)
  namelist /fndata/ fnyy, fnmm, fndd, fnsensor, fnversion, ndyy, ndmm, nddd, ndsensor, ndversion

  ! 3) directories
  character (len=128) :: idir1    ! input directory
  character (len=128) :: idir2    ! input directory
  character (len=128) :: odir    ! output directory
  namelist /dirnames/ idir1, idir2, odir

  ! ----------------------------------------------------------------
  ! Begin Program Code
  ! ----------------------------------------------------------------

  ! ----------------------------------------------------------------
  ! define remaining header vars
  ! ----------------------------------------------------------------
  earth_radius = 6378.273
  is_wind_grid_rel = .FALSE.
  field = 'XICE'
  units = 'fraction'
  map_source = 'NSIDC'
  desc = 'Bootstrap sea ice, daily'

  ! ----------------------------------------------------------------
  ! read in namelist file
  ! ----------------------------------------------------------------
  open(unit=nlunit, file='namelist.input', status='old')
  read(unit=nlunit, nml=rundata)
  read(unit=nlunit, nml=fndata)
  read(unit=nlunit, nml=dirnames)
  len_i1 = len_trim( idir1 )
  len_i2 = len_trim( idir2 )
  len_o = len_trim( odir )
  close(unit=nlunit)
  print *,hemi,' ',ni,' ',nj,' ',truelat1,' ',xlonc,' '
  print *,startlat,' ',startlon,' ',startloc
  print *,fnyy,' ',fnmm,' ',fndd,' ',fnsensor, ndyy,' ',ndmm,' ',nddd,' ',ndsensor
!  print *,'idir1=',idir1(1:len_i1)
!  print *,'idir2=',idir2(1:len_i2)
!  print *,'odir=',odir(1:len_o)

  ! ----------------------------------------------------------------
  ! allocate dynamic data arrays
  ! ----------------------------------------------------------------
  nelem = nj*ni
  allocate( idata(nelem) )
  allocate( slab00( ni, nj ) )
  allocate( slab06( ni, nj ) )
  allocate( slab12( ni, nj ) )
  allocate( slab18( ni, nj ) )
  allocate( slab24( ni, nj ) )

  ! ----------------------------------------------------------------
  ! create input file names
  ! ----------------------------------------------------------------
  write( dynfmt, '("(A",I3.3,",''/bt_'',A4,A2,A2,''_'',A3,''_'',A3,''_'',A1,''.bin'')")') len_i1
  write( ifn1,dynfmt ) idir1(1:len_i1),fnyy, fnmm, fndd, fnsensor, fnversion, hemi
  write( dynfmt, '("(A",I3.3,",''/bt_'',A4,A2,A2,''_'',A3,''_'',A3,''_'',A1,''.bin'')")') len_i2
  write( ifn2,dynfmt ) idir2(1:len_i2),ndyy, ndmm, nddd, ndsensor, ndversion, hemi
!  1001 format(A,'/bt_',A4,A2,A2,'_',A3,'_v02_',A1,'.bin')
  k = len_trim( ifn1 )
  print *,'ifn1=',ifn1(1:k)
  k = len_trim( ifn2 )
  print *,'ifn2=',ifn2(1:k)

  ! ----------------------------------------------------------------
  ! read in data files
  ! ----------------------------------------------------------------
  call read_file( iunit, ifn1, nelem, slab00 )
  call read_file( iunit, ifn2, nelem, slab24 )

  ! ----------------------------------------------------------------
  ! process 00Z == input file 1
  ! ----------------------------------------------------------------
  fnhh = 0
  write( hdate,1003 ) fnyy, fnmm, fndd, fnhh
  1003 format(A4,'-',A2,'-',A2,'_',I2.2,':00:00')
  print *,hdate

  ! ----------------------------------------------------------------
  ! create output file name
  ! ----------------------------------------------------------------
  write( dynfmt, '("(A",I3.3,",''/XICE:'',A4,''-'',A2,''-'',A2,''_'',I2.2)")') len_o
  write( ofn,dynfmt ) odir(1:len_o),fnyy, fnmm, fndd, fnhh
!  1002 format(A,'/XICE:',A4,'-',A2,'-',A2,'_00')
  k = len_trim( ofn )
  print *,'ofn=',ofn(1:k)

  ! ----------------------------------------------------------------
  ! write out 00Z
  ! ----------------------------------------------------------------
  call write_file( ounit, ofn, version, slab00, is_wind_grid_rel, &
                   hdate, xfcst, map_source, field, units, &
                   desc, xlvl, ni, nj, iproj, startloc, startlat, &
                   startlon, deltalat, deltalon, earth_radius, dx, dy, &
                   truelat1, truelat2, xlonc, nlats )

  ! ----------------------------------------------------------------
  ! process 12Z == average of today and next files
  ! ----------------------------------------------------------------
  fnhh = 12
  write( hdate,1003 ) fnyy, fnmm, fndd, fnhh
  print *,hdate

  ! ----------------------------------------------------------------
  ! create output file name
  ! ----------------------------------------------------------------
  write( dynfmt, '("(A",I3.3,",''/XICE:'',A4,''-'',A2,''-'',A2,''_'',I2.2)")') len_o
  write( ofn,dynfmt ) odir(1:len_o),fnyy, fnmm, fndd, fnhh
!  1002 format(A,'/XICE:',A4,'-',A2,'-',A2,'_00')
  k = len_trim( ofn )
  print *,'ofn=',ofn(1:k)

  ! ----------------------------------------------------------------
  ! calculate 12Z
  ! ----------------------------------------------------------------
  slab12 = (slab00 + slab24) / 2

  ! ----------------------------------------------------------------
  ! write out 12Z
  ! ----------------------------------------------------------------
  call write_file( ounit, ofn, version, slab12, is_wind_grid_rel, &
                   hdate, xfcst, map_source, field, units, &
                   desc, xlvl, ni, nj, iproj, startloc, startlat, &
                   startlon, deltalat, deltalon, earth_radius, dx, dy, &
                   truelat1, truelat2, xlonc, nlats )

  ! ----------------------------------------------------------------
  ! process 06Z == average of today and 12Z
  ! ----------------------------------------------------------------
  fnhh = 6
  write( hdate,1003 ) fnyy, fnmm, fndd, fnhh
  print *,hdate

  ! ----------------------------------------------------------------
  ! create output file name
  ! ----------------------------------------------------------------
  write( dynfmt, '("(A",I3.3,",''/XICE:'',A4,''-'',A2,''-'',A2,''_'',I2.2)")') len_o
  write( ofn,dynfmt ) odir(1:len_o),fnyy, fnmm, fndd, fnhh
!  1002 format(A,'/XICE:',A4,'-',A2,'-',A2,'_00')
  k = len_trim( ofn )
  print *,'ofn=',ofn(1:k)

  ! ----------------------------------------------------------------
  ! calculate 06Z
  ! ----------------------------------------------------------------
  slab06 = (slab00 + slab12) / 2

  ! ----------------------------------------------------------------
  ! write out 06Z
  ! ----------------------------------------------------------------
  call write_file( ounit, ofn, version, slab06, is_wind_grid_rel, &
                   hdate, xfcst, map_source, field, units, &
                   desc, xlvl, ni, nj, iproj, startloc, startlat, &
                   startlon, deltalat, deltalon, earth_radius, dx, dy, &
                   truelat1, truelat2, xlonc, nlats )

  ! ----------------------------------------------------------------
  ! process 18Z == average of 12Z and next
  ! ----------------------------------------------------------------
  fnhh = 18
  write( hdate,1003 ) fnyy, fnmm, fndd, fnhh
  print *,hdate

  ! ----------------------------------------------------------------
  ! create output file name
  ! ----------------------------------------------------------------
  write( dynfmt, '("(A",I3.3,",''/XICE:'',A4,''-'',A2,''-'',A2,''_'',I2.2)")') len_o
  write( ofn,dynfmt ) odir(1:len_o),fnyy, fnmm, fndd, fnhh
!  1002 format(A,'/XICE:',A4,'-',A2,'-',A2,'_00')
  k = len_trim( ofn )
  print *,'ofn=',ofn(1:k)

  ! ----------------------------------------------------------------
  ! calculate 18Z
  ! ----------------------------------------------------------------
  slab18 = (slab12 + slab24) / 2

  ! ----------------------------------------------------------------
  ! write out 18Z
  ! ----------------------------------------------------------------
  call write_file( ounit, ofn, version, slab18, is_wind_grid_rel, &
                   hdate, xfcst, map_source, field, units, &
                   desc, xlvl, ni, nj, iproj, startloc, startlat, &
                   startlon, deltalat, deltalon, earth_radius, dx, dy, &
                   truelat1, truelat2, xlonc, nlats )

! ----------------------------------------------------------------
! End of mainline code, Start of subroutine code
! ----------------------------------------------------------------

CONTAINS
  ! ----------------------------------------------------------------
  ! read_file: read in binary sea ice data
  ! ----------------------------------------------------------------
  subroutine read_file( iunit, ifn, nelem, slab )

  IMPLICIT none

  integer, intent(in) :: iunit, nelem
  character(len=*), intent(in) :: ifn
  real, dimension(:,:), intent(out) :: slab

  integer :: i, j, k, L

  open(unit=iunit, file=ifn, status='old', &
       recl=nelem, access='direct', form='unformatted')
  print *,'reading the data'
  read(unit=iunit,rec=1) idata
  close(unit=iunit)

  ! ----------------------------------------------------------------
  ! Convert data format:
  ! - 1-d to 2-ds
  ! - divide by 1000 to be fraction not percentage (divide by 10)
  ! Based on code from
  ! https://nsidc.org/data/pm/bootstrap-seaice-fortran
  ! ----------------------------------------------------------------
  ! The following shows the four ways to copy the data from the 1-d
  ! original var (data) to the 2-d processed data (slab).
  ! -- v1:    slab(i,j)=float(data(L))/1000.
  !           *** data will be flipped top/bottom
  ! -- v2:    slab(ni-i+1,j)=float(data(L))/1000.
  !           *** data will be flipped top/bottom & left/right
  ! -- v3:    slab(i,nj-j+1)=float(data(L))/1000.
  !           *** data will be correct for SH
  ! -- v4:    slab(ni-i+1,nj-j+1)=float(data(L))/1000.
  !           *** data will be flipped left/right
  ! ----------------------------------------------------------------
  print *,'transforming the data'
  do 100 i=1,ni
    do 100 j=1,nj
      L = ni*(j-1)+i
  !   if (hemi == 's') then
        k = nj-j+1    ! SH & NH?
  !   else
  !     k = j         ! NH?
  !   end if
      slab(i,k) = float(idata(L))/1000.   ! -- v3
  !   print *,i,' ',j,' ',L,' ',k
  ! ----------------------------------------------------------------
  100 continue
  where( slab > 1.0 )
    slab = 0.0
  endwhere

  return
  end subroutine read_file

  ! ----------------------------------------------------------------
  ! write_file: create the intermediate file
  ! ----------------------------------------------------------------
  subroutine write_file( ounit, ofn, version, slab, is_wind_grid_rel, &
                         hdate, xfcst, map_source, field, units, &
                         desc, xlvl, ni, nj, iproj, startloc, startlat, &
                         startlon, deltalat, deltalon, earth_radius, dx, dy, &
                         truelat1, truelat2, xlonc, nlats )

  IMPLICIT none

  integer, intent(in) :: ounit, version, ni, nj, iproj

  real, dimension(:,:), intent(in) :: slab
  real, intent(in) :: startlat, startlon, deltalat, deltalon, earth_radius
  real, intent(in) :: dx, dy, truelat1, truelat2, xlonc, nlats, xfcst, xlvl

  logical, intent(in) :: is_wind_grid_rel

  character(len=*), intent(in) :: ofn
  character(len=*), intent(in) :: hdate
  character(len=*), intent(in) :: map_source
  character(len=*), intent(in) :: field
  character(len=*), intent(in) :: units
  character(len=*), intent(in) :: desc
  character(len=*), intent(in) :: startloc

  open(unit=ounit, file=ofn, status='replace', form='unformatted')

  !  1) WRITE FORMAT VERSION
  write(unit=ounit) version

  !  2) WRITE METADATA
  ! Cylindrical equidistant
  if (iproj == 0) then
        write(unit=ounit) hdate, xfcst, map_source, field, &
                          units, desc, xlvl, ni, nj, iproj
        write(unit=ounit) startloc, startlat, startlon, &
                          deltalat, deltalon, earth_radius

  ! Mercator
  else if (iproj == 1) then
        write(unit=ounit) hdate, xfcst, map_source, field, &
                          units, desc, xlvl, ni, nj, iproj
        write(unit=ounit) startloc, startlat, startlon, dx, dy, &
                          truelat1, earth_radius

  ! Lambert conformal
  else if (iproj == 3) then
        write(unit=ounit) hdate, xfcst, map_source, field, &
                          units, desc, xlvl, ni, nj, iproj
        write(unit=ounit) startloc, startlat, startlon, dx, dy, &
                          xlonc, truelat1, truelat2, earth_radius

  ! Gaussian
  else if (iproj == 4) then
        write(unit=ounit) hdate, xfcst, map_source, field, &
                          units, desc, xlvl, ni, nj, iproj
        write(unit=ounit) startloc, startlat, startlon, &
                                 nlats, deltalon, earth_radius

  ! Polar stereographic
  else if (iproj == 5) then
        write(unit=ounit) hdate, xfcst, map_source, field, &
                          units, desc, xlvl, ni, nj, iproj
        write(unit=ounit) startloc, startlat, startlon, dx, dy, &
                          xlonc, truelat1, earth_radius
  end if

  !  3) WRITE WIND ROTATION FLAG
  write(unit=ounit) is_wind_grid_rel

  !  4) WRITE 2-D ARRAY OF DATA
  write(unit=ounit) slab

  close(unit=ounit)

  return
  end subroutine write_file

end program read_data
