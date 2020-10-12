(* 컴퓨터공학부/2009-11679/김정명/7 *)

type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

let check m =
	let rec iter (m, l) =
		match m with
		V s -> 
			List.mem s l
		| P (a, me) -> 
			iter(me, l @ [a])
		| C (m1, m2) ->
			iter (m1, l) && iter (m2, l)
	in
	iter (m, [])
