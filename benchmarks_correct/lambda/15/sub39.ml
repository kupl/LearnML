type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec contains ((n: var), (l: var list)): bool = (*List.contains function*)
	match l with 
	| [] -> false
	| h::t -> if (n=h) then true
			  else contains(n, t)

let rec checksub((m: lambda), (l: var list)): bool = 
	match m with
	| V n -> contains(n, l)
	| P (n, m) -> checksub(m, [n] @ l)
	| C(m1, m2) -> checksub (m1, l) && checksub (m2, l)

let check (m: lambda): bool = 
	match m with
	| V n -> false 
	| P (n, m) -> checksub (m, [n])
	| C (m1, m2) -> checksub (m1, []) && checksub (m2, [])
