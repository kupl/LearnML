type lambda = V of var
	 | P of var * lambda
	 | C of lambda * lambda
 and var = string

let rec check(lambda, discovered) = 
	match lambda with
 	V(n) -> List.mem n discovered
	| P(n,m) -> check(m, n::discovered) 
	| C(m1,m2) -> check(m1,discovered) && check(m2,discovered)

let check(lambda) = 
	check(lambda, [])

	
