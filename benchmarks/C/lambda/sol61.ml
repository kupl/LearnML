type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check m =
	let rec cmhelper m l =
		match m with
			| V(a) -> List.exists (function x -> a = x) l
			| P(a,b) -> cmhelper b (a :: l)
			| C(a,b) -> (cmhelper a l) && (cmhelper b l)
	in
	cmhelper m []