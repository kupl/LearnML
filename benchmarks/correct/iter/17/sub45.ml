let rec iter = fun (n, f) ->
	if n=0 then fun x -> x
	else fun x -> iter(n-1,f) (f x)
(*
let _ = print_int ((iter(10, fun x -> x+2) 0))
let _ = print_newline()
*)
