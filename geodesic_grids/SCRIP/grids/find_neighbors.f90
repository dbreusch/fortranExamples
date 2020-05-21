        PROGRAM find_neighbors

        use netcdf
        implicit none

        integer :: fileID, ncid
        integer :: center_lat_id, center_lon_id
        integer :: corner_lat_id, corner_lon_id
        integer :: status
        integer :: grid_size, ncorners
        integer :: i, j, k, m
        integer :: cnt
        double :: min_lon, max_lon
        double :: x0, y0, x1, y1, x2, y2
        logical :: found

        double, dimension(:), allocatable :: center_lat
        double, dimension(:), allocatable :: center_lon
        double, dimension(:,:), allocatable :: corner_lat
        double, dimension(:,:), allocatable :: corner_lon
        integer, dimension(:,:), allocatable :: neighbors
        logical, dimension(:), allocatable :: match

! open the grid file
        status = nf_open (gridfile, NF_NOWRITE, fileID)
        call handle_error (status, "file = "//gridfile)

! get grid dimensions
        status = nf_inq_dimid (fileID, "grid_size", ncid)
        call handle_error (status, "grid_size")
        status = nf_inq_dimlen (fileID, ncid, grid_size)
        call handle_error (status, "grid_size")

! get grid corners
        status = nf_inq_dimid (fileID, "grid_corners", ncid)
        call handle_error (status, "grid_corners")
        status = nf_inq_dimlen (fileID, ncid, ncorners)
        call handle_error (status, "grid_corners")

! allocate space for data arrays
        allocate( center_lat( grid_size ) )
        allocate( center_lon( grid_size ) )
        allocate( corner_lat( grid_size, ncorners ) )
        allocate( corner_lon( grid_size, ncorners ) )
        allocate( neighbors( grid_size, ncorners ) )

! read center latitudes
        status = nf_inq_varid (fileID, "grid_center_lat", center_lat_id)
        call handle_error (status, "center_lat")
        status = nf_get_var_double (fileID, center_lat_id, center_lat)

! read center longitudes
        status = nf_inq_varid (fileID, "grid_center_lon", center_lon_id)
        call handle_error (status, "center_lon")
        status = nf_get_var_double (fileID, center_lon_id, center_lon)
        call handle_error (status, "center_lon")

! read corner latitudes
        status = nf_inq_varid (fileID, "grid_corner_lat", corner_lat_id)
        call handle_error (status, "corner_lat")
        status = nf_get_var_double (fileID, corner_lat_id, corner_lat)

! read corner longitudes
        status = nf_inq_varid (fileID, "grid_corner_lon", corner_lon_id)
        call handle_error (status, "corner_lon")
        status = nf_get_var_double (fileID, corner_lon_id, corner_lon)
        call handle_error (status, "corner_lon")

! find min/max lon, to be able to handle wrap-around
        min_lon = min( center_lat )
        max_lon = max( center_lat )

! loop through the grid points
        do i=1,grid_size
          x0 = center_lon(i)
          y0 = center_lat(i)
          cnt = 0

! loop through this cell's corners
          do j=1,ncorners
            x1 = corner_lon(i,j)
            y1 = corner_lat(i,j)

! loop through all the other corners
            do k=1,ncorners
              match = any( corner_lat(:,k) .eq. y1 .and. corner_lon(:,k) .eq. x1 )
              do m=1,grid_size
                if (match(m) .eq. .true.) ! matching point found
                  if (m .eq. i) ! skip self-match
                    goto 1000
                  end if
                  if (cnt .eq. 0) ! first match
                    cnt=cnt+1
                    neighbors( i, cnt ) = m
                  else  ! not first match
                    found = .false.
                    do n=1,cnt
                      if (neighbors( i,n ) .eq. m)
                        found = .true.
                        goto 1000
                      end if
                    end do  ! n
                    if (.not. found)
                      cnt=cnt+1
                      neighbors( i, cnt ) = m
                    end if
                  end if  ! cnt
 1000             continue
                end if  ! match(m)
              end do ! m

              ! extra checks for vertical edges
              if (x0 .eq. min_lon)
              ! at left-hand side
              end if
              if (x0 .eq. max_lon)
              ! at right-hand side
              end if

            end do ! k
         end do ! j
       end do ! i

       END PROGRAM find_neighbors
