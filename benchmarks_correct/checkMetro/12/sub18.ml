(* ex8 *)
type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

let check m = 
	let rec checker (met, lst) = 
		match met with
		  P (str, met_) -> checker (met_, str::lst)
		| C (met1, met2) -> (checker (met1, lst)) && (checker (met2, lst))
		| V str -> List.mem str lst
	in
	checker (m, [])