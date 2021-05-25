(* hw 1_2. *)
let rec iter (n, f) a =
	if n = 0 then a
	else f (iter(n-1, f) a)

	(*
let _ =
	iter(10, function x -> 2 + x) 0
	*)
