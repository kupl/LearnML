type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) =
	match (nat1, nat2) with
	| (ZERO, nat2) -> nat2
	| (nat1, ZERO) -> nat1
	| (SUCC n1, _) -> SUCC (natadd (n1, nat2))

let rec natmul (nat1, nat2) =
	match (nat1, nat2) with
	| (ZERO, nat2) -> ZERO
	| (nat1, ZERO) -> ZERO
	| (SUCC n1, _) -> natadd (nat2, natmul(n1, nat2))