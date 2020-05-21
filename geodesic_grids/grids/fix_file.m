j=1;
c=zeros(size(r,1)/2,6);
for i=1:2:(size(r,1)-1)
  a=r(i,:);
  id=find(a==0);
  id=find(id);
  ll=length(id);
  if (ll==1)
    a=r(i,1:3);
  else
    a=r(i,:);
  end
  b=r(i+1,:);
  id=find(b==0);
  id=find(id);
  ll=length(id);
  if (ll==2)
    b=r(i+1,1:2);
  else
    b=r(i+1,1:3);
  end
  c(j,:)=[a b];
  j=j+1;
end