(* Problem 1 *)
exception Problem
let rec sigma f a b =

	if a<0||b<0||a>b then raise Problem
	else
 
if a == b then f b

else f b + sigma f a (b-1);;



