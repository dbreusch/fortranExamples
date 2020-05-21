load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
begin
  r2d = 57.2957795             ; radians to degrees
  in = addfile("./geotest.nc","r")
	zs = in->zs
  x  = in->grid_center_lon * r2d
  y  = in->grid_center_lat * r2d
  cx = in->grid_corner_lon * r2d
  cy = in->grid_corner_lat * r2d
;	printVarSummary(zs)
;	printVarSummary(x)
;	printVarSummary(y)
;	printVarSummary(cx)
;	printVarSummary(cy)

  wks = gsn_open_wks("ps","geotest")
	gsn_define_colormap(wks,"gui_default")

	res = True
;  res@mpProjection      = "Mercator"       ; choose projection
;  res@mpGridAndLimbOn   = True              ; turn on lat/lon lines
;  res@mpPerimOn         = False             ; turn off box around plot
;  res@mpGridLatSpacingF = 20.               ; spacing for lat lines
;  res@mpGridLonSpacingF = 30.               ; spacing for lon lines
;  res@mpFillOn          = False
 
  res@cnFillOn          = True              ; color plot desired
  res@cnLineLabelsOn    = False             ; turn off contour lines
;  res@cnLevelSelectionMode = "ManualLevels"        ; manual contour levels
;  res@cnLevelSpacingF      = 400                   ; contour interval
;  res@cnMinLevelValF       = -50.                  ; min level
;  res@cnMaxLevelValF       =  5000.
  res@txFontHeightF     = 0.015 
  res@lbLabelBarOn      = True

  res@vpXF            = 0.1                 ; make plot bigger
  res@vpYF            = 0.9         
  res@vpWidthF        = 0.8
  res@vpHeightF       = 0.8

  res@gsnSpreadColors     = True            ; use full range of color map
  res@gsnSpreadColorStart = 2 
  res@gsnSpreadColorEnd   = -3 
  res@lbLabelFontHeightF  = 0.015           ; label bar font height
 
  res@tiMainString       = "geotest.nc"   ; add a title
  res@tiMainFontHeightF  = .018                               ; font height

; these resources are what set the triangular mesh.
  res@sfXArray             = x
  res@sfYArray             = y
  res@sfXCellBounds        = cx
  res@sfYCellBounds        = cy

  contour = gsn_csm_contour_map(wks,zs,res)  ; create the plot
end
