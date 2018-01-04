type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
	match n1 with
	| ZERO -> n2
	| SUCC n1' -> SUCC (natadd (n1', n2))

let rec natmul (n1, n2) =
	match (n1, n2) with
	| (ZERO, n2) -> ZERO
	| (SUCC n1', n2) -> natadd(n2, natmul (n1', n2))
