let rec sigma f a b  =
	if a>b then 0
	else (f a) + (sigma f (a+1) b)
(*
let add1 x = x+1
let _ = print_endline (string_of_int (sigma (0, 3, add1)))
*)
