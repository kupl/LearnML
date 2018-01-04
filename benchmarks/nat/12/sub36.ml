type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
	match (n1, n2) with
	| (ZERO, n2) -> n2
	| (SUCC n, n2) -> SUCC(natadd(n, n2))

let rec natmul (n1, n2) =
	match (n1, n2) with
	| (n1, ZERO) -> ZERO
	| (ZERO, n2) -> ZERO
	| (n1, SUCC ZERO) -> n1
	| (n1, SUCC n) -> natadd(n1, natmul(n1, n))
