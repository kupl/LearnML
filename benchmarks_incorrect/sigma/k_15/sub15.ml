(* Problem 1 *)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
	if b = a then a
	else
		f b + sigma f a (b-1)
