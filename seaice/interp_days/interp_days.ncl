; use NCL to interpolate binary sea ice day and day+2 files to day+1

begin
  if (.not. isdefined( "day1" ) )
    print("day1 not defined...exiting...")
    exit
  end if
  if (.not. isdefined( "hemi" ) )
    print("hemi not defined...defaulting to nh")
    hemi = "nh"
  end if
  
  basediri = "/glade/work/dbr/data/seaice/bin_"+hemi
  basediro = "/glade/work/dbr/data/seaice/bin_"+hemi+"_i"

  ; parse day1 to yy/mm/dd
  yy1 = str_get_cols(day1, 0, 3)
  mm1 = str_get_cols(day1, 4, 5)
  dd1 = str_get_cols(day1, 6, 7)

  ; calculate day+1, day+2
  iyy1 = stringtointeger( yy1 )
  TIME = yyyymmdd_time( iyy1, iyy1+1, "integer" )
  day1ix = ind( TIME .eq. stringtointeger( day1 ))
  day2ix = day1ix + 1
  day3ix = day2ix + 1
  day2 = tostring(TIME(day2ix))
  day3 = tostring(TIME(day3ix))

  ; get filename for day1
  delim = "/"
  idir1 = basediri+"/"+yy1
  ipatt1 = idir1+"/bt_"+yy1+mm1+dd1+"_*.bin"
  fn1 = systemfunc( "ls "+ipatt1 )
  nfields = str_fields_count(fn1, delim)
  fn1b = str_get_field(fn1, nfields, delim) ; get actual filename

  ; get filename for day3
  yy3 = str_get_cols(day3, 0, 3)
  mm3 = str_get_cols(day3, 4, 5)
  dd3 = str_get_cols(day3, 6, 7)
  idir3 = basediri+"/"+yy3
  ipatt3 = idir3+"/bt_"+yy3+mm3+dd3+"_*.bin"
  fn3 = systemfunc( "ls "+ipatt3 )
  nfields = str_fields_count(fn3, delim)
  fn3b = str_get_field(fn3, nfields, delim) ; get actual filename

  ; create filename for day2 by combining day2 time info
  ; and part of day1 file info
  delim = "_"
  f3 = str_get_field(fn1b, 3, delim)
  f4 = str_get_field(fn1b, 4, delim)
  f5 = str_get_field(fn1b, 5, delim)
;  print(f3+" "+f4+" "+f5)
  fn2b = "bt_"+day2+"_"+f3+"_"+f4+"_"+f5
  yy2 = str_get_cols(day2, 0, 3)
  idir2 = basediro+"/"+yy2
  fn2 = idir2+"/"+fn2b
  print("day1: "+fn1b)
  print("day2: "+fn2b)
  print("day3: "+fn3b)

; skip interpolation if output file actually exists
  fn2_test = idir1+"/"+fn2b ; check day1 dir
  if (fileexists(fn2_test)) then
    print("Output file "+fn2_test+" exists, not overwriting!")
    exit
  end if
  f3b = str_get_field(fn3b, 3, delim)
  f4b = str_get_field(fn3b, 4, delim)
  f5b = str_get_field(fn3b, 5, delim)
  fn2b_test = "bt_"+day2+"_"+f3b+"_"+f4b+"_"+f5b
  fn2_test = idir3+"/"+fn2b_test ; check day3 dir
  if (fileexists(fn2_test)) then
    print("Output file "+fn2_test+" exists, not overwriting!")
    exit
  end if

;  print("fn1: "+fn1)
;  print("fn2: "+fn2)
;  print("fn3: "+fn3)
;  exit
  
  ; check system endian-ness
  if (isbigendian() ) then
    print("System is bigendian")
    setfileoption("bin","ReadByteOrder","LittleEndian")
    setfileoption("bin","WriteByteOrder","LittleEndian")
;  else
;    print("System is littleendian")
  end if

  ; read input files
  sic1 = cbinread( fn1, -1, "float" )
  sic3 = cbinread( fn3, -1, "float" )

  ; interpolate
  sic2 = (sic1 + sic3) / 2

  ; write out new file
  cbinwrite( fn2, sic2 )
end
