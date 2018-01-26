exception Problem;;

let rec pascal (x, y) =
	if x<0 || y<0 then raise Problem
	else if x < y then raise Problem
	else if y=0 then 1
	else if x=y then 1
	else pascal(x-1, y-1) + pascal(x-1, y);;
