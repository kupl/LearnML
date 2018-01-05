let rec sigma : (int * int * (int -> int)) -> int = fun (a, b, f) ->
	if a>b then 0
	else (f a) + (sigma (a+1, b, f))
(*
let add1 x = x+1
let _ = print_endline (string_of_int (sigma (0, 3, add1)))
*)
