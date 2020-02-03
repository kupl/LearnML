type lambda = V of var
	 | P of var * lambda
	 | C of lambda * lambda
 and var = string

let rec check2(lambda, discovered) = 
	match lambda with
 	V(n) -> List.mem n discovered
	| P(n,m) -> check2(m, n::discovered) 
	| C(m1,m2) -> check2(m1,discovered) && check2(m2,discovered)

let check(lambda) = 
	check(lambda, [])

	
