exception Error of string

let rec sigma (a, b, f)= 
	if b < a then raise (Error "Error")
	else if b = a then f a
	else f a +. sigma (a+1, b, f)	

let sumprod (matrix, n, k) =
	sigma(1, n, (function x -> sigma(1, k, (function y -> matrix (x, y))) ))



(*
let a (i, j) = 
	match (i, j) with
	(1,1) -> 1.0
	| (1,2) -> 2.0
	| (2,1) -> 3.0
	| (2,2) -> 4.0
	| _ -> 5.0
*)
