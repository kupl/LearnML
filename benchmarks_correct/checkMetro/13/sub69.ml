type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda
and var = string

let rec check_ varlist m =
	match m with
	| V(id) -> (List.mem id varlist)
	| P(id, m) -> (check_ (id::varlist) m)
	| C(m1, m2) -> (check_ varlist m1 && check_ varlist m2)
	
let check m = (check_ [] m)
