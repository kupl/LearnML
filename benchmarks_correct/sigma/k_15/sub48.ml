let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
	if a = b then f b
	else if a > b then 0
	else sigma f a (b-1) + f b

