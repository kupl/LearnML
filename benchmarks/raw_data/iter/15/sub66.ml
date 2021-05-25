let rec iter (n,f) = match n with
	|0 -> fun k -> k
	|1 -> f
	|x -> fun k -> f (iter (x-1,f) k)
(*
let du = iter (10, fun x -> 2+x)
let _ = print_endline (string_of_int (du 2) )
*)
