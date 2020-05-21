package Make_KML;

# create Placemarks from an input text file
sub make_pm {
  my $ifn = shift( @_ );  # input file
  my $iconstyle = shift( @_ );  # style for icons
  my $delta = shift( @_ );  # Region half-dimension
  my $aref = shift( @_ ); # ref to lats array
  my @lats = @$aref;
  $aref = shift( @_ ); # ref to lons array
  my @lons = @$aref;
  my $minpix = shift( @_ );  # minimum Level of Detail
  my $maxpix = shift( @_ );  # maximum Level of Detail

  my ($id, $label, $lat, $lon);

  Make_KML::make_folder($ifn);
  open( IFILE, "< $ifn");
  my @ids = <IFILE>;
  close(IFILE);

  my $gpt=1;
  foreach $id (@ids) {
    $label = sprintf( "%d", $id );
    $lat = $lats[$id];
    chomp($lat);
    $lon = $lons[$id];
    if ($lon > 180) {
      $lon -= 360;
      }
    chomp($lon);

    Make_KML::make_folder($gpt);
    Make_KML::make_region( $lat, $lon, $delta, $minpix, $maxpix );
    Make_KML::make_region();

    # remove leading whitespace
    $lat =~ s/^\s+//;
    $lon =~ s/^\s+//;

    printf STDOUT "  <Placemark>\n";
#    printf STDOUT "    <name>Gridpoint $gpt</name>\n";
    printf STDOUT "    <description>\n";
    printf STDOUT "    Geodesic gridpoint $label at $lat, $lon\n";
    printf STDOUT "    </description>\n";
    printf STDOUT "    <styleUrl>#$iconstyle</styleUrl>\n";
    printf STDOUT "    <Point>\n";
    printf STDOUT "      <coordinates>$lon,$lat,0</coordinates>\n";
    printf STDOUT "    </Point>\n";
    printf STDOUT "  </Placemark>\n";
    $gpt++;
    Make_KML::make_folder();
    }

  Make_KML::make_folder();
  return 0;
  }

sub make_hdr {
  printf STDOUT "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  printf STDOUT "<kml xmlns=\"http://earth.google.com/kml/2.1\">\n";
  printf STDOUT "<Document>\n";
  Make_KML::make_style( 1, 0.6, "ffffffff" ); # white
  Make_KML::make_style( 2, 0.6, "ff0000ff" ); # red
  Make_KML::make_style( 3, 0.5, "ff00ff00" ); # green
  Make_KML::make_style( 4, 0.4, "ffff0000" ); # blue

  return 0;
  }

sub make_style {
  my $count = shift( @_ );
  my $scale = shift( @_ );
  my $color = shift( @_ );

  printf STDOUT "  <Style id=\"iconstyle$count\">\n";
  printf STDOUT "    <BalloonStyle>\n";
  printf STDOUT "      <text>\$[description]</text>\n";
  printf STDOUT "    </BalloonStyle>\n";
  printf STDOUT "    <IconStyle>\n";
  printf STDOUT "      <color>$color</color>\n";
  printf STDOUT "      <scale>$scale</scale>\n";
  printf STDOUT "      <Icon id=\"Icon_ID$count\">\n";
  printf STDOUT "        <href>http://maps.google.com/mapfiles/kml/shapes/target.png</href>\n";
  printf STDOUT "      </Icon>\n";
  printf STDOUT "    </IconStyle>\n";
  printf STDOUT "  </Style>\n";

  return 0;
  }

sub make_folder {
  my $desc = shift( @_ );
  if (defined $desc) {
    printf STDOUT "  <Folder>\n";
    printf STDOUT "    <name>$desc</name>\n";
    }
  else {
    printf STDOUT "  </Folder>\n";
    }

  return 0;
  }

sub make_region {
  my $clat = shift( @_ );
  my $clon = shift( @_ );
  my $delta = shift( @_ );
  my $minpix = shift( @_ );
  my $maxpix = shift( @_ );

  my $x;
  if (defined($clat)) {
    printf STDOUT "    <Region>\n";
    printf STDOUT "      <LatLonAltBox>\n";
    $x=$clat+$delta;
    printf STDOUT "        <north>$x</north>\n";
    $x=$clat-$delta;
    printf STDOUT "        <south>$x</south>\n";
    $x=$clon-$delta;
    printf STDOUT "        <west>$x</west>\n";
    $x=$clon+$delta;
    printf STDOUT "        <east>$x</east>\n";
    printf STDOUT "      </LatLonAltBox>\n";
    printf STDOUT "      <Lod>\n";
    if ($minpix>0) {
      printf STDOUT "        <minLodPixels>$minpix</minLodPixels>\n";
      }
    if (defined($maxpix)) {
      printf STDOUT "        <maxLodPixels>$maxpix</maxLodPixels>\n";
      }
    printf STDOUT "      </Lod>\n";
    }
  else {
    printf STDOUT "    </Region>\n";
    }

  return 0;
  }

# note: the following line is needed to make "require" work
1
