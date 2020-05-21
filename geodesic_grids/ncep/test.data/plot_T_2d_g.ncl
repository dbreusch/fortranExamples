; plot geodesic T field, one day, one level
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
;  idir = "/local/model-i/dbr/ncep/test.data"
  idir = "/creche/dbr/ncep/test.data"

; air.20000629.700.regrid.nc
;  var_root = "air"
;  var_name = var_root
;  vartitle = "Temperature"
;  date = "20000629"
;  level = 700

; air.20000927.500.regrid.nc
;  date = "20000927"
;  level = 500

; air.sig995.19960118.regrid.nc
;  var_root = "air.sig995"
;  var_name = "air"
;  date = "19960118"
;  level = "sfc"

; uwnd.19960118.700.regrid.nc
  var_root = "uwnd"
  var_name = var_root
  vartitle = "u-wind"
  date = "19960118"
  level = 700

; ----------------

  if (isinteger(level))
    ifn = var_root+"."+date+"."+level+".regrid"
  else
    ifn = var_root+"."+date+".regrid"
  end if
  print("Input from "+ifn)
  in = addfile( idir+"/"+ifn+".nc", "r")
	d = in->$var_name$
  lvl = in@level
  tt = in@time
  year = tt / 1000000
  month = (tt - (year*1000000)) / 10000
  day = (tt - (year*1000000) - (month*10000)) / 100
  tstr=year+"-"+sprinti("%02d",month)+"-"+sprinti("%02d",day)
  print("Plotting time "+tstr)

	r2d = 57.2957795 ; radians to degrees
  x  = in->grid_center_lon * r2d
  y  = in->grid_center_lat * r2d
  cx = in->grid_corner_lon * r2d
  cy = in->grid_corner_lat * r2d

  ofn = idir+"/"+ifn
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

	res@tiMainString       = "Geodesic "+lvl+" "+vartitle+", "+tstr   ; add a title

; these resources are what set the triangular mesh.
  res@sfXArray             = x
  res@sfYArray             = y
  res@sfXCellBounds        = cx
  res@sfYCellBounds        = cy

;	plot = gsn_csm_contour_map(wks, d({20:55}, {230:310}), res)  ; create the plot
	plot = gsn_csm_contour_map(wks, d, res)  ; create the plot

end
