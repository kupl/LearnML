type nat = ZERO | SUCC of nat

let rec natadd(n1, n2) : nat =
	match n1 with
	| ZERO -> n2
	| SUCC(n1_minus_one) -> natadd(n1_minus_one, SUCC(n2))

let rec natmul(n1, n2) : nat =
	match n1 with
	| ZERO -> ZERO
	| SUCC(n1_minus_one) -> natadd(n2, natmul(n1_minus_one, n2))





