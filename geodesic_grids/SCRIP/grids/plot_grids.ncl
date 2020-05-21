; the goal of this script was to create 3 plots showing grid points:
; 1 - polar stereographic for NH
; 2 - Mercator for whole globe
; 3 - polar stereographic for SH
; but unfortunately there's either something wrong in NCL or in how
; I'm going about this because trying to do more than one leads NCL
; to emit error messages and either die or not create proper plots.
;  
; in lieu of fixing these issues, the script has been changed to just
; do one global plot, in either Mercator or Mollweide
;
; change ifn and suff to select input grid file and it's output suffix

load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_csm.ncl"

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

  wks = gsn_open_wks("ps","grid"+suff)
  gsn_define_colormap(wks, "BlueRed")
  r2d = 57.2957795 ; radians to degrees
  mx = max( ctrlat )
  if (mx .lt. 10) ; data in radians
    ctrlat = ctrlat * r2d
    ctrlon = ctrlon * r2d
    corlat = corlat * r2d
    corlon = corlon * r2d
  end if

  gsres = True
  gsres@gsMarkerIndex = 16
  gsres@gsMarkerColor = 2
  gsres@gsMarkerSizeF = 0.003

  plot = new(3, graphic)
  stride=1

; causes problems when combined w/other plots
;  print("Plot 1")
;  res = True
;  res@gsnDraw               = False
;  res@gsnFrame              = False  ; do not advance frame
;  res@pmTickMarkDisplayMode = "Always"
;;  res@gsnCenterString = title
;  res@gsnMaximize = True
;  res@gsnPolar = "NH"
;  plot(0) = gsn_csm_map_polar( wks, res )
;  lon_sub = mask( ctrlon(0:(npoints-1):stride), (ctrlat(0:(npoints-1):stride) .ge. 0), True )
;  lat_sub = mask( ctrlat(0:(npoints-1):stride), (ctrlat(0:(npoints-1):stride) .ge. 0), True )
;  pts = gsn_add_polymarker( wks, plot(0), lon_sub, lat_sub, gsres )
;  delete(pts)
;  delete(lon_sub)
;  delete(lat_sub)
;  delete(res)

; also causes problems when combined w/other plots but ok alone
  print("Plot 2")
  res = True
  res@gsnDraw               = False
  res@gsnFrame              = False  ; do not advance frame
  res@pmTickMarkDisplayMode = "Always"
  res@gsnMaximize = True
  res@mpProjection = "Mollweide"
;  res@mpProjection = "Mercator"
  plot(1) = gsn_csm_map( wks, res )
  pts = gsn_add_polymarker( wks, plot(1), ctrlon(0:(npoints-1):stride), ctrlat(0:(npoints-1):stride), gsres )
;  delete(pts)
;  delete(res)

; causes problems when combined w/other plots
;  print("Plot 3")
;  res = True
;  res@gsnDraw               = False
;  res@gsnFrame              = False  ; do not advance frame
;  res@pmTickMarkDisplayMode = "Always"
;  res@gsnMaximize = True
;  res@gsnPolar = "SH"
;  plot(1) = gsn_csm_map_polar( wks, res )
;  lon_sub = mask( ctrlon(0:(npoints-1):stride), (ctrlat(0:(npoints-1):stride) .le. 0), True )
;  lat_sub = mask( ctrlat(0:(npoints-1):stride), (ctrlat(0:(npoints-1):stride) .le. 0), True )
;  pts = gsn_add_polymarker( wks, plot(1), lon_sub, lat_sub, gsres )

; paneling not needed when just doing one plot
;  print("Paneling")
;  resPanel=True
;  resPanel@txString = "6-Hourly Data"
;  gsn_panel( wks, plot, (/3,1/), False)
  draw( wks )
  frame( wks )

end
