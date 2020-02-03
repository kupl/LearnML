type lambda =
	V of var
	| P of var * lambda
	| C of lambda * lambda
	and var = string
let rec check2 lambda varlist =
	match lambda with
	| V var -> List.exists (fun x -> x = var) varlist
	| P (a, b) -> check2 b (a::varlist)
	| C (a, b) -> (check2 a varlist) && (check2 b varlist)
let check lambda =
	check lambda []