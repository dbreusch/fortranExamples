function N=find_neigh( M, n_nbr,rpt_int )
% find the neighbors of all points in grid M
% Input:
%   M (e.g., NCEP, geo)
%     col 1:2 = center lat, lon
%     col 3:(ncorners+2) = lat_corners
%     col (ncorners+3):(ncorners*2+2) = lon_corners
%   n_nbr = maximum number of neighbors expected per grid point
%   rpt_int = progress reporting interval (default 100)
%
% Output
%   N = (npts,n_nbr)
%     col n = nth neighbor for grid point i

if nargin < 2
  disp('Error:  must specify input grid and number of neighbors')
  N=[];
  return
end
if nargin < 3
  rpt_int=100;
end

npts=size(M,1);
ncols=size(M,2);
min_lon=min(M(:,2));
max_lon=max(M(:,2));
N=zeros(size(M,1),n_nbr);

cor=M(1,3:ncols);
ncor=size(cor,2)/2;
c1=3; c2=c1+ncor-1;
c3=c2+1; c4=ncols;
clat=M(:,c1:c2);
clon=M(:,c3:c4);

for i=1:npts
  cy=M(i,1);
  cx=M(i,2);
  cor=M(i,3:ncols);
%   disp([num2str(cy),',',num2str(cx)])
  
  nb_id=[];
  for j=1:ncor
    y=cor(j);
    x=cor(j+ncor);
%     disp([' ',num2str(y),',',num2str(x)])
    for cc=1:ncor
      id=find(clat(:,cc)==y & clon(:,cc)==x);
      if (cx==min_lon)
        id2=find(clat(:,cc)==y & clon(:,cc)==(x+360));
        id=[id id2];
      end
      if (cx==max_lon)
        id2=find(clat(:,cc)==y & clon(:,cc)==(x-360));
        id=[id id2];
      end
        
      fnd=0;
      for k=1:length(id)
  %       disp(['  ',num2str(id(k)),': ',num2str(clat(id(k),cc)),',',num2str(clon(id(k),cc))])
        if id(k)==i
          fnd=1;
          break
        end
        for kk=1:length(nb_id)
          if id(k)==nb_id(kk)
            fnd=1;
            break;
          end % if
        end % for kk
        if fnd
          break
        end % if
      end % for k
      if ~fnd
        nb_id=[nb_id id(k)];
      end %if
    end % for cc
  end % for j
%   disp(num2str(nb_id))
  % for j=1:length(nb_id)
  %   disp([num2str(M(nb_id(j),1)),',',num2str(M(nb_id(j),2))])
  % end
  N(i,1:length(nb_id))=nb_id;
  if (mod(i,rpt_int)==0)
    disp([num2str(i),': ',num2str(cy),',',num2str(cx),' ',num2str(length(nb_id))])
  end
end % i