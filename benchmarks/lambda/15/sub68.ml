type lambda 	= V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let rec validMetro (m , vars) =
	match m with
	| V var -> List.exists (fun x -> x = var) vars
	| P (var , m0) -> validMetro(m0,var::vars)
	| C (m1,m2) -> validMetro(m1,vars) && validMetro(m2,vars)

let check m = 
	validMetro(m,[])
