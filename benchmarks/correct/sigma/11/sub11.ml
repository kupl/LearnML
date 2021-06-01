let rec sigma f a b =
	if a < b then sigma f a (b-1) + f b
	else if a = b then f b
	else raise (Invalid_argument "sigma")
(*
let _ = let result = sigma(1,5,fun x -> x + 2) in print_int result;
	print_newline();
*)
