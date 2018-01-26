let rec pascal (x,y) = if (x<0)||(y<0) then raise (Failure "inserted numbers cannot be negative")
else if x<y then raise (Failure "first value cannot be smaller than second value") 
else if x=y then 1
else if y=0 then 1
else pascal (x-1,y-1) + pascal (x-1,y);;
