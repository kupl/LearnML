(* 200511843 LEE JONGHO *)

let rec iter (n, f) =
	match n with
	0 -> 0 
	| _ -> f (iter(n-1, f))