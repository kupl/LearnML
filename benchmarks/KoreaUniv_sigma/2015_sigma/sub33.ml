let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
let rec sigma f a b =
	if a = b then f b
	else if a > b then 0
	(* else if b > a then -( ( f a ) + (sigma f a+1 b) ) *)
	else if a < b then ( f a ) + (sigma f a+1 b);;	
