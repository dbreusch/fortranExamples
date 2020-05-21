; original NCL script to find neighbors for all gridpoints
; this DOES NOT WORK so be sure to use find_neigh.m!

begin
  ifn = "./NCEP_2.5deg_grid.nc"
  suff = "_NCEP"
;  ifn = "./C10242.global.nc"
;  suff = "_geod"
  G = addfile(ifn, "r" )
  ctrlat = G->grid_center_lat
  ctrlon = G->grid_center_lon
  corlat = G->grid_corner_lat
  corlon = G->grid_corner_lon
  npoints = dimsizes( ctrlat )
  npoints_half = npoints / 2
  d = dimsizes( corlat )
  ncorners = d(1)
  print("Size="+npoints)
  print("n Corners="+ncorners)

  ;r2d = 57.2957795 ; radians to degrees
  ;mx = max( ctrlat )
  ;if (mx .lt. 10) ; data in radians
  ;  ctrlat = ctrlat * r2d
  ;  ctrlon = ctrlon * r2d
  ;  corlat = corlat * r2d
  ;  corlon = corlon * r2d
  ;end if
  eps = 0.01
  pi = 3.141592654
  pi2 = 2*pi
  nomatch=0

  Ni=new( (/ npoints, ncorners /), "integer" )
  Ni@_FillValue = -999
  Ni = -999
  NiOpt = True
  NiOpt@fout= "neighbors_i"+suff+".txt"

  Nj=new( (/ npoints, ncorners /), "integer" )
  Nj@_FillValue = -999
  Nj = -999
  NjOpt = True
  NjOpt@fout= "neighbors_j"+suff+".txt"

;  do i=0,npoints-1 ; loop on npoints
  do i=600,601 ; loop on npoints
    x0 = ctrlon(i)
    y0 = ctrlat(i)
    print("Center "+i+"= ("+y0+","+x0+")")
    do j=0,ncorners-1 ; loop on ncorners
      if (.not. ismissing( Ni(i,j) ) )
        continue
      end if
      x1 = corlon(i,j)
      if (x1 .lt. 0)
        x1 = x1 + pi2
      end if
      y1 = corlat(i,j)
      print("Corner("+i+","+j+")=("+y1+","+x1+")")
      found = False
      di = 10
      old_di_lo = npoints+1
      old_di_hi = -npoints-1
      do while (.not. found)
        lb=i-di
        ub=i+di
;        print(lb+" "+old_di_lo+" "+old_di_hi+" "+ub)
        do k=lb,ub  ; inner loop on npoints
          if (k .gt. old_di_lo .and. k .lt. old_di_hi)
            continue
          end if
          if (k .lt. 0 .or. k .gt. (npoints-1))
            continue
          end if
          if ( k .eq. i )
            continue
          end if
;          print("k="+k)
          mindy=9999999.0
          mindx=9999999.0
          do l=0,ncorners-1 ; inner loop on ncorners
;            print("k="+k+" l="+l)
            y2=corlat(k,l)
            x2=corlon(k,l)
;            print("  Corner("+k+","+l+")=("+y2+","+x2+")")
            dx = fabs(x2-x1)
            if (dx .lt. mindx)
              mindx = doubletofloat(dx)
            end if
            dy = fabs(y2-y1)
            if (dy .lt. mindy)
              mindy = doubletofloat(dy)
            end if
            if ( dy .le. eps .and. dx .le. eps )
              Ni(i,j) = k
              Nj(i,j) = l
              Ni(k,l) = i
              Nj(k,l) = j
              found = True
              break
            end if
          end do  ; l
          if (found .eq. True)
            break
          end if
        end do ; k
        if (found .eq. True)
        print("Match found at ("+k+","+l+")=("+corlat(k,l)+","+corlon(k,l)+")")
        else
          if (di .lt. npoints_half)
            old_di_lo = i-di
            old_di_hi = i+di
;            print("Expanding di ("+old_di_lo+","+old_di_hi+")")
            di = di * 2
;            if (di .gt. npoints_half)
;              di = npoints_half - 1
;            end if
          else
            print("No Match found at ("+i+","+j+")=("+y1+","+x1+")")
            nomatch=nomatch+1
            print("mindx="+mindx+" mindy="+mindy)
            if (nomatch .gt. 1)
              exit
            end if
            break
          end if
        end if
      end do ; while
    end do ; j
  end do ; i

;  write_matrix( Ni, "6i", NiOpt )
;  write_matrix( Nj, "6i", NjOpt )
end
