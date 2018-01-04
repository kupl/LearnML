type nat = ZERO | SUCC of nat

let rec natadd ((n1 : nat), (n2 : nat)) : nat =
	match n2 with
	|ZERO -> n1
	|SUCC(n2_minus_one) -> natadd (SUCC(n1), n2_minus_one)

let rec natmul ((n1 : nat), (n2 : nat)) : nat =
	match n2 with
	|ZERO -> ZERO
	|SUCC(n2_minus_one) -> natadd (n1, natmul (n1, n2_minus_one))