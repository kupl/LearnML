exception Error of string

(* EX1 : sigma *)
let rec sigma( a, b, f ) =
	if a > b then raise ( Error "invalid input" )
	(* error case : a, b are negative or zero, or a is greater than b *)
	else if a = b then f(a)
	(* base case : return f(a) where a = b *)
	else f(a) + (sigma ( (a+1), b, f ))
