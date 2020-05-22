load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

; use NCL to convert sea ice binary files to netCDF files

begin
  ; command line arguments
  ; ifn is required
  if (.not. isdefined( "ifn" ) )
    print("Must provide ifn argument.")
    exit
  end if
  ; yr is optional but will probably always be used
  if (.not. isdefined( "yr" ) )
    print("Argument yr absent, defaulting to 1988.")
    yr = "1988"
  end if

  ; hemisphere is optional but will probably always be used
  if (.not. isdefined( "hem" ) )
    print("Argument hem(isphere) absent, defaulting to sh.")
    hem = "sh"
  end if

  infile = "bin_"+hem+"/"+yr+"/"+ifn+".bin"

  ; define the type (byte order) of binary data
  setfileoption("bin","ReadByteOrder","LittleEndian")

  ; define file dimensions
  if (hem .eq. "sh")
    ni = 332
    nj = 316
    ll_fn = "s"
  end if
  if (hem .eq. "nh")
    ni = 448
    nj = 304
    ll_fn = "n"
  end if

  ; process the latitude file
  ll_dir = "/glade/u/home/dbr/user/data/seaice/"
  v = cbinread(ll_dir+"ps"+ll_fn+"25lats_v3.dat",-1,"integer")
  l = v / 100000.
  l2d = onedtond( l, (/ni,nj/) )
  lat = l2d(::-1,:) ; flip "up-down"
  lat@long_name = "Latitude"
  lat@standard_name = "latitude"
  lat@units = "degrees_north"
  dlat  = dimsizes(lat)

  ; process the longitude file
  v = cbinread(ll_dir+"ps"+ll_fn+"25lons_v3.dat",-1,"integer")
  l = v / 100000.
  l2d = onedtond( l, (/ni,nj/) )
  lon = l2d(::-1,:) ; flip "up-down"
  lon@long_name = "Longitude"
  lon@standard_name = "longitude"
  lon@units = "degrees_east"

  ; process the sea ice data
  v = cbinread(infile,-1,"short")
  l = v / 10.
  l = l / 100.
  l2d = onedtond( l, (/ni,nj/) )
  pct2d = l2d(::-1,:) ; flip "up-down"
  pct = conform_dims( (/1,ni,nj/), pct2d,(/1,2/) )
  pct@_FillValue = default_fillvalue("float")
  pct@long_name = "Sea ice fraction"
  pct@units = ""
  pct@coordinates = "lon lat"
  ; values > 1 are land, change them to fill values
  pct = where( pct .gt. 1.0, pct@_FillValue, pct )
;  printVarSummary( pct )

  ; parse ifn for time vars
  yy = stringtoint( str_get_cols( ifn, 3, 6 ) )
  mm = stringtoint( str_get_cols( ifn, 7, 8 ) )
  dd = stringtoint( str_get_cols( ifn, 9, 10 ) )
  print(yy+" "+mm+" "+dd)
  
  ; calculate time (as days), using 1900 as arbitrary start reference
  units = "days since 1900-01-01 00:00:00"
  t = cd_inv_calendar( yy, mm, dd, 0, 0, 0, units, 0 )
  t@long_name = "Time"
  t@standard_name = "time"

  ; parse ifn for sensor name
  sensor =  str_get_cols( ifn, 12, 14 )

  ; create the netCDF output file
  diro = "nc_"+hem+"/"+yr+"/" ; output directory
  filo = ifn+".nc"    ; output filename
  system("/bin/rm -f " + diro + filo)    ; remove if exists
  fout  = addfile (diro + filo, "c")  ; open output file
  setfileoption(fout,"DefineMode",True)

  ; global attributes
  fAtt = True
  fAtt@title = "Bootstrap sea ice concentration"
  fAtt@source_file = ifn
  fAtt@sensor = sensor
  fAtt@url = "http://nsidc.org/data/docs/daac/nsidc0079_bootstrap_seaice.gd.html"
  fAtt@Conventions = "None"
  fAtt@creation_date = systemfunc ("date")
  fileattdef( fout, fAtt ) ; copy file attributes

  ; define file dimensions
  dimNames = (/"time", "i", "j"/)
  dimSizes = (/ -1   ,  ni,  nj /)
  dimUnlim = (/ True , False, False/)
  filedimdef(fout,dimNames,dimSizes,dimUnlim)
  
  ; define vars
  filevardef(fout, "time"  ,typeof(t), (/"time"/) )
  filevarattdef( fout,"time",t )

  filevardef(fout, "lat"  ,typeof(lat), (/"i", "j"/) )
  filevarattdef( fout,"lat",lat )

  filevardef(fout, "lon"  ,typeof(lon), (/"i", "j"/) )
  filevarattdef( fout,"lon",lon )

  filevardef(fout, "seaice"  ,typeof(pct), (/"time","i", "j"/) )
  filevarattdef( fout,"seaice",pct )

  setfileoption(fout,"DefineMode",False)

  ; write the vars
  fout->time   = (/t/)
  fout->lat    = (/lat/)
  fout->lon    = (/lon/)
  fout->seaice = (/pct/)
end
