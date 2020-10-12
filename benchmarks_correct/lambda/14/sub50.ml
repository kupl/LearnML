type var =
string

type lambda =
  V of var
| P of var * lambda
| C of lambda * lambda

let check m =
	let rec check1 m city =
	match m with
	  V n -> List.mem n city
	| P (n, m1) -> let city1 = n::city in check1 m1 city1
	| C (m1, m2) -> check1 m1 city && check1 m2 city
	in
check1 m []
