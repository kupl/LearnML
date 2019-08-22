type lambda = V of var
			|P of var * lambda
			|C of lambda * lambda
and var = string

let rec check((m:lambda)) =
	let rec check ((ma:lambda), (li:var list))=
		match ma with
		|P(st, k) -> check(k, st::li)
		|C(me1, me2) -> check(me1, li) && check(me2, li)
		|V(na) -> List.mem na li
	in
	check(m, [])
