type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) : nat=
	match nat1 with
	| ZERO -> nat2
	| SUCC(a) -> natadd(a, SUCC(nat2))
	
let rec natmul (nat1, nat2) :nat =
	match nat1 with
	| ZERO -> nat1
	| SUCC(a) -> natadd(natmul(a, nat2), nat2)

