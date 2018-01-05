
(* Exercise 2*)
let rec sigma : int * int * (int -> int) -> int = fun (a, b, f1) ->
	if a > b then 0
	else if a == b then f1 b
	else (f1 a) + (sigma (a+1, b, f1)) 
