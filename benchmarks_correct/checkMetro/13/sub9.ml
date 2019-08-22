type lambda =
	V of var
	| P of var * lambda
	| C of lambda * lambda
	and var = string
let rec check lambda varlist =
	match lambda with
	| V var -> List.exists (fun x -> x = var) varlist
	| P (a, b) -> check b (a::varlist)
	| C (a, b) -> (check a varlist) && (check b varlist)
let check lambda =
	check lambda []