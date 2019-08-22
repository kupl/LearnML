type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec cMIter: lambda * var list -> bool = fun (m, l) ->
	match m with
	V(n) -> List.mem n l
	| P(n, nm) -> cMIter (nm, n::l)
	| C(a, b) -> (cMIter (a, l)) && (cMIter (b, l))

let check: lambda->bool = fun m ->
	cMIter (m, [])
