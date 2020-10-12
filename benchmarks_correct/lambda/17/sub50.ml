type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let rec withlist (m: lambda) (nl: string list): bool = 
	match m with
	| P(n, me) -> (withlist me (n::nl))
	| C(m1, m2) -> (withlist m1 nl) && (withlist m2 nl)
	| V(n) -> (List.mem n nl)

let rec check (m: lambda): bool =
	(withlist m []) 