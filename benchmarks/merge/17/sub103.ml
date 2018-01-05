
(* Exercuse 1 *)
let rec merge : int list * int list -> int list = fun (l1,l2) -> 
	match l2 with
	| [] -> l1
	| h2::t2 -> 
		match l1 with
		| [] -> l2
		| h1 :: t1 ->
			if h1 > h2 then h1 :: merge (t1,h2::t2)
			else h2 :: merge(h1::t1,t2)
