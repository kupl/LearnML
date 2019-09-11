(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #3 *)
type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let rec checkAux (m, l) : bool = 
	match m with
	| P(x, y) -> checkAux(y, x::l)
	| C(x, y) -> checkAux(x, l) && checkAux(y, l)
	| V x -> List.mem x l

let rec check (m : lambda) : bool = checkAux(m, [])
