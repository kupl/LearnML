
type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec subCheck(s,a) =
	match s with
	V x -> 
		if List.mem x a then true
		else false
	| P (var, s2) -> subCheck(s2, var::a)
	| C (s2, s3) -> subCheck(s2, a) && subCheck(s3, a)
	

let rec check s =
	let alist = [] in
	subCheck(s, alist)
			

