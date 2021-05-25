(* hw 1_1. *)
let rec sigma (a, b, f) =
	if a = b then f a
	else f a + sigma((a + 1), b, f)

	(*
let _ =
	sigma(1, 10, (function x -> 2 * x))
	*)
