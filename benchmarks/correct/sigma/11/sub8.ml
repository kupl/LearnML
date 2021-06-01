(* hw 1_1. *)
let rec sigma f a b =
	if a = b then f a
	else f a + sigma f (a+1) b

	(*
let _ =
	sigma(1, 10, (function x -> 2 * x))
	*)
