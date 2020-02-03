type lambda = V of var
			|P of var * lambda
			|C of lambda * lambda
and var = string

let rec check((m:lambda)) =
	let rec check2 ((ma:lambda), (li:var list))=
		match ma with
		|P(st, k) -> check2(k, st::li)
		|C(me1, me2) -> check2(me1, li) && check2(me2, li)
		|V(na) -> List.mem na li
	in
	check2(m, [])
