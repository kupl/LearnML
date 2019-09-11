(* 2008-11874 EXERCISE 8 *)

type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec check lambda =
	let rec varStation lambda =
		match lambda with
			| V(n) -> n::[]
			| P(n,m) -> varStation m
			| C(m1,m2) -> List.append (varStation m1) (varStation m2)
	in
	
	let rec varArea lambda =
		match lambda with
			| V(n) -> []
			| P(n,m) -> n::(varArea m)
			| C(m1,m2) -> List.append (varArea m1) (varArea m2)
	in
	
	let rec check l1 l2 =
		match l1 with
			| [] -> true
			| hd::tl -> (List.mem hd l2) && (check tl l2)
	in
	
	check (varStation lambda) (varArea lambda)
	
	