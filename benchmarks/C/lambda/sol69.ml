
type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda
and var = string

let rec checklist(me, li) =
	match me with
	| V n -> List.mem n li
	| P(n1, me1) -> checklist (me1, n1::li)
	| C(me1,me2) -> checklist(me1, li) && checklist(me2, li)


let rec check me =
	let l = [] in
		checklist(me, l)


