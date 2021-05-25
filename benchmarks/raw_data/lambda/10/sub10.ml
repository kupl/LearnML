type var = string 

type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda

;;

let rec check2 (me, li) =
	match me with
		V n -> List.mem n li
		| P (n, m) -> check2 (m, (n :: li))
		| C (m1, m2) -> check2 (m1, li) && check2 (m2, li)

let check me = check2 (me, [])

;;	