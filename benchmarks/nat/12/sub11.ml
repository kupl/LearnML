type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
	match (n1, n2) with
	| (ZERO, a) -> a
	| (a, ZERO) -> a
	| (_, SUCC a) -> SUCC (natadd (n1, a))

let rec natmul (n1, n2) =
	match (n1, n2) with
	| (ZERO, a) -> ZERO
	| (a, ZERO) -> ZERO
	| (_, SUCC a) -> natadd (n1, natmul (n1, a))

