type var = string 

type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda

;;

let rec check (me, li) =
	match me with
		V n -> List.mem n li
		| P (n, m) -> check (m, (n :: li))
		| C (m1, m2) -> check (m1, li) && check (m2, li)

let check me = check (me, [])

;;	