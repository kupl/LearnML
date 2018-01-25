(*********************)
(* Problem 1: filter *)
(*********************)

let rec filter : ('a -> bool) -> 'a list -> 'a list
=fun pred lst -> match lst with
									| [] -> []
									| x::tail -> if (pred x)
																 then x::(filter pred tail)
															else (filter pred tail)
