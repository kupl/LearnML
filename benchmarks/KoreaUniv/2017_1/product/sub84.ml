exception Problem;;

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
	if a=b then f(a)
	else if a<b then (product f a (b-1))*(f b)
	else raise Problem;;