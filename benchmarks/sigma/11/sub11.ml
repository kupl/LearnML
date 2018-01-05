let rec sigma : int * int * (int -> int) -> int = fun (a, b, f) ->
	if a < b then sigma (a, b-1, f) + f b
	else if a = b then f b
	else raise (Invalid_argument "sigma")
(*
let _ = let result = sigma(1,5,fun x -> x + 2) in print_int result;
	print_newline();
*)
