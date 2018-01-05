(* 1 merge: int list * int list -> int list *)
let rec merge (l0, l1) = match l0, l1 with
	| [], _ -> l1
	| _, [] -> l0
	| h0::t0, h1::t1 -> if h0 > h1 then h0::(merge (t0, l1))
					else h1::(merge (l0, t1))
