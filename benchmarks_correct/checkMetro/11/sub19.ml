
(* ex 7 *)

type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda 

and var = string

let check met =
	let rec checkList(s,l) =
		match l with
		| [] -> false
		| h::t -> if s=h then true else checkList(s,t)
	in	

	let rec cal(m,l) =
		match m with 
		| V n -> checkList(n,l) 
		| P(n,m1) -> cal(m1,n::l) 
		| C(m1,m2) -> cal(m1,l) && cal(m2,l)
	in

	cal(met,[])


