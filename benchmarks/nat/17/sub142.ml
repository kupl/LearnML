type nat = ZERO | SUCC of nat

let rec natadd ((nat1 : nat),(nat2 : nat)) : nat =
	match nat1 with
	|ZERO -> nat2
	|SUCC(nat1_minus_1) -> natadd(nat1_minus_1,SUCC(nat2)) 

let rec natmul ((nat1 : nat),(nat2 : nat)) : nat =
	match nat1 with
	|ZERO -> nat1
	|SUCC(nat1_minus_1) -> natadd(natmul(nat1_minus_1,nat2),nat2)

