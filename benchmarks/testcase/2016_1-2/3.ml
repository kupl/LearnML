let rec f : int -> int -> int
= fun x y -> 
	if x=0 || y=x then 1
	else (f (x-1) (y-1)) + (f (x-1) y);;
