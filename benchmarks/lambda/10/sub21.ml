type lambda = V of var
	     | P of var * lambda
	     | C of lambda * lambda
 and var = string

let rec check met =
	let rec check2 met lst = 
	match met with 
	V(id) -> List.mem id lst 
	| P(id, m) -> check2 m (lst @ [id])
	| C(m1, m2) -> check2 m1 lst & check2 m2 lst
	in
	match met with 
	V(id) -> false
	| P(id, m) -> check2 m [id] 
	| C(m1, m2) -> false
	;;


