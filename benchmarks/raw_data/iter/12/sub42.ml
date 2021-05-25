let rec iter (n, f) =
	if n == 0
		then (function x -> x)
		else (function x -> f (iter (n-1,f) x) )

(*
let a x = x + 2

let _ = print_int ( (iter 10 a) 0 )
*)
