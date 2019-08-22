(* 2006-11377 hw2-1 *)

type lambda = 
V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let check lambda = 
	let rec cm (arlist, met) = 
		match met with
		| V var -> (List.mem var arlist)
		| P (avar, m) -> cm(avar::arlist, m)
		| C (m1, m2) -> cm(arlist, m1) && cm(arlist, m2)
	in	
	match lambda with
	| V _ | C _ -> cm([], lambda)
	| P (avar, m) -> cm([avar], m)
