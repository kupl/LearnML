type lambda = 	V of var
		| P of var * lambda
		| C of lambda * lambda

and var = string


let rec check (m: lambda) : bool =
	let rec checkArea((mc: lambda), (l: string list)) : bool =
		match mc with
		| V nd -> List.mem nd l
		| C (md1, md2) -> checkArea(md1, l) && checkArea(md2, l)
		| P (nd, md) -> checkArea(md, (List.append l [nd]))
		
	in
	match m with
	| V n -> false
	| C (m1, m2) -> check(m1) && check(m2)
	| P (ns, ms) -> checkArea(m, [])
