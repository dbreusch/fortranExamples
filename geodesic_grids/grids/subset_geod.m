function ix=subset_geod( lat, lon, lat_sp, lon_sp )
% ix=subset_geod( lat, lon, lat_sp, lon_sp )

if nargin < 4
  lon_sp = lat_sp;
end

n=1;
lat = lat * pi / 180;
lon = lon * pi / 180;

new_lats = -90:lat_sp:90;
for i=1:length(new_lats)
  clat = new_lats(i);
  if (abs(clat) < 90)
    adj = cos( clat * pi / 180 );
    lon_sp_adj = lon_sp / adj;  % increase long spacing with latitude
    new_lons = 0:lon_sp_adj:(360-lon_sp_adj/2);
  else
    new_lons=0;
  end
  clat = clat * pi / 180;

  for j=1:length(new_lons)
    clon = new_lons(j);
    clon = clon * pi / 180;
    dlat = 1e-3;
    id1=[];
    while isempty(id1)
      x1 = clat-dlat;
      x2 = clat+dlat;
      x3 = clon-dlat;
      x4 = clon+dlat;
    
      id1=find((lat >= x1) & (lat <= x2) & (lon >=x3) & (lon <= x4));
      if ~isempty(id1)
        break;
      else
        dlat=dlat*2;
      end
    end
      
% now have id vector of nearby lat and lon values
% need to find closest
    mindist=Inf;
    for k=1:length(id1)
      dist = m_lldist( [clon lon(id1(k))], [clat lat(id1(k))] );
      if dist < mindist
        mindist=dist;
        min_id = id1(k);
      end
    end
%     disp(num2str(mindist))
    ix(n) = min_id-1;
    n=n+1;
  end
end
