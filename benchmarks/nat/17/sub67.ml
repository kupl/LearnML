type nat = ZERO | SUCC of nat

let rec natadd ((n1: nat), (n2: nat)): nat =
	match (n1, n2) with
	| (ZERO, n2) -> n2
	| (n1, ZERO) -> n1
	| ((SUCC(n11)), (SUCC(n22))) -> natadd(SUCC(n1), n22)

let rec natmul ((n1: nat), (n2: nat)): nat =
	match (n1, n2) with
	| (ZERO, n2) -> ZERO
	| (n1, ZERO) -> ZERO
	| (SUCC(n11), SUCC(n22)) -> natadd(n1, natmul(n1, n22))

