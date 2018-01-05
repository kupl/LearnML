type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) =
	match nat1 with
		| ZERO -> nat2
		| SUCC nat -> SUCC (natadd (nat, nat2))

let rec natmul (nat1, nat2) =
	match nat1 with
		| ZERO -> ZERO
		| SUCC nat -> natadd(natmul(nat, nat2), nat2)


