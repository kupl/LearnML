type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

let rec checklst lst m =
	match m with
	V id -> (List.mem id lst)
	| P (id, m1) -> (checklst (id::lst) m1)
	| C (m1, m2) -> (checklst lst m1) && (checklst lst m2)

let check m =
	(checklst [] m)
