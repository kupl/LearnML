(*
	department : computer science & engineering
	student ID : 2012-11242 / var : Seon-bi, Park
*)

type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let rec checking mtr arealst = 			(* lambda -> list -> bool*)
	match mtr with
		| V(n) ->
		if (List.mem n arealst) = true then true
		else false
		| C(m,n) -> (checking m arealst) && (checking n arealst)
		| P(n,m) -> (checking m (n::arealst))

let check mtr = 					(* lambda -> bool *)
	match mtr with
		| V(n) -> false
		| C(m,n) -> (checking m []) && (checking n [])
		| P(n,m) -> (checking mtr [])
