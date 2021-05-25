exception Error of string

(* EX2 : iter *)
let rec iter( n, f ) =
	if n < 0 then raise ( Error "invaild input" )
	(* error case : n is negative *)
	else if n = 0 then fun x -> x
	(* base case : return x -> x where n = 0 *)
	else fun x -> iter( n-1, f ) ( f x )
