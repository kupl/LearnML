type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda
and var = string;;

let check met =
	let rec areaChecker (lst : string list) m b = 
		match m with
		| V n -> b && (List.mem n lst)
		| P (n, m1) -> b && (areaChecker (n::lst) m1 b)
		| C (m1, m2) -> b && (areaChecker lst m1 b) && (areaChecker lst m2 b) in
	areaChecker [] met true;;
