(* Problem 1 *)
let rec pascal ( x , y ) =
	if y<0 then 0 else(*exception*)
	if y>x then 0 else 
	if y=0 then 1 else
	if y=x then 1 else
	pascal(x-1,y-1) + pascal(x-1,y);;
