type nat = ZERO | SUCC of nat

let rec natadd ((n1: nat), (n2: nat)) : nat  =
	match (n1, n2) with
	| (ZERO, _) -> n2
	| (_, ZERO) -> n1
	| (SUCC(i), SUCC(j)) -> SUCC(SUCC(natadd(i, j))) 

let rec natmul ((n1: nat), (n2: nat)) : nat =
	match (n1, n2) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (i, SUCC(j)) -> natadd(i, natmul(i, j))

