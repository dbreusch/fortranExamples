; plot geodesic T field, one day, one level, 3-d input
; use this to plot interpolated fields/grids
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
;  idir = "/local/model-i/dbr/ncep/test.data"
  idir = "/creche/dbr/ncep/test.data"

; air.1996.daily.700.regrid.nc
  var_root = "air"
  var_name = var_root
  vartitle = "Temperature"
;  in_yr = 1996
;  t_ix = 101
  level = 700

; air.2000.daily.700.regrid.nc
  in_yr = 2000
;  t_ix = 245
;  t_ix = 31
  t_ix = 1

; air.1995.daily.regrid.nc
; in_yr = 1995
; t_ix = 31

; air.sig995.1993.daily.regrid.nc
;  var_root = "air.sig995"
;  var_name = "air"
;  in_yr=1993
;  t_ix=53
;  level = "sfc"

; ----------------

	t_ix=t_ix - 1 ; adjust for zero-base
  if (isinteger(level))
    ifn = var_root+"."+in_yr+".daily."+level+".regrid"
  else
    ifn = var_root+"."+in_yr+".daily.regrid"
  end if
	print("Input from "+ifn)
  in = addfile( idir+"/"+ifn+".nc", "r")
	d = in->$var_name$(t_ix,:)

  tt = d@time
  tt@units = "hours since 1-1-1 00:00:0.0"
  utc_date = ut_calendar( tt, 0 )
  year = floattointeger( utc_date(:,0))
  month = floattointeger( utc_date(:,1))
  day = floattointeger( utc_date(:,2))
  tstr=year+"-"+sprinti("%02d",month)+"-"+sprinti("%02d",day)
  tstr2=sprinti("%02d",month)+sprinti("%02d",day)

  utc_date = ut_calendar( tt, 0 )
  year = floattointeger( utc_date(:,0))
  month = floattointeger( utc_date(:,1))
  day = floattointeger( utc_date(:,2))
  tstr=year+"-"+sprinti("%02d",month)+"-"+sprinti("%02d",day)
  print("Plotting time "+tstr)

	r2d = 57.2957795 ; radians to degrees
  x  = in->grid_center_lon * r2d
  y  = in->grid_center_lat * r2d
  cx = in->grid_corner_lon * r2d
  cy = in->grid_corner_lat * r2d

  if (isinteger(level))
    ofn = idir+"/"+var_root+"."+in_yr+".daily."+tstr2+"."+level+".regrid"
  else
    ofn = idir+"/"+var_root+"."+in_yr+".daily."+tstr2+".regrid"
  end if
  print("Output to "+ofn+".ps")
  wks = gsn_open_wks("ps",ofn)
	gsn_define_colormap(wks,"BlueRed")

	res = True
;  res@mpProjection      = "Mollweide"       ; choose projection
;  res@mpProjection      = "LambertConformal"       ; choose projection
  res@mpProjection      = "Mercator"       ; choose projection
;	res@gsnMaskLambertConformal = True
	res@gsnAddCyclic = False
  res@mpGridAndLimbOn   = True              ; turn on lat/lon lines
  res@mpPerimOn         = False             ; turn off box around plot
  res@mpGridLatSpacingF = 20.               ; spacing for lat lines
  res@mpGridLonSpacingF = 30.               ; spacing for lon lines
  res@mpFillOn          = False
;	res@mpMinLatF         = 20
;	res@mpMaxLatF         = 55
;	res@mpMinLonF         = -130
;	res@mpMaxLonF         = -50
;	res@mpLimitMode       = "LatLon"
 
  res@cnFillOn          = True              ; color plot desired
  res@cnLineLabelsOn    = False             ; turn off contour lines
;  res@cnLevelSelectionMode = "ManualLevels"        ; manual contour levels
;  res@cnLevelSpacingF      = 400                   ; contour interval
;  res@cnMinLevelValF       = -50.                  ; min level
;  res@cnMaxLevelValF       =  5000.
  res@txFontHeightF     = 0.015 
  res@lbLabelBarOn      = True
  res@lbLabelAutoStride      = True
  res@lbLabelAngleF      = 45

  res@gsnMaximize = True
  res@gsnSpreadColors     = True            ; use full range of color map
  res@gsnSpreadColorStart = 2 
  res@gsnSpreadColorEnd   = -3 
  res@lbLabelFontHeightF  = 0.015           ; label bar font height
 
  res@tiMainFontHeightF  = .018                               ; font height

;	res@tiMainString       = "NCEP "+lvl+" Temperature, "+t   ; add a title

; these resources are what set the triangular mesh.
  res@sfXArray             = x
  res@sfYArray             = y
  res@sfXCellBounds        = cx
  res@sfYCellBounds        = cy

;	plot = gsn_csm_contour_map(wks, d({20:55}, {230:310}), res)  ; create the plot
  res@tiMainString       = "Geodesic "+level+" "+vartitle+", "+tstr   ; add a title
	plot = gsn_csm_contour_map(wks, d, res)  ; create the plot

end
