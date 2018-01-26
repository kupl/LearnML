let rec pascal (x,y)=
if x=0 || x=1 || x=y || y=0 then 1
else
    pascal(x-1, y-1) + pascal(x-1, y) 

