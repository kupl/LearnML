type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
	match (a, b) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC nat) -> (SUCC (natadd (a, nat)))
	| (SUCC nat, ZERO) -> (SUCC (natadd (nat, b)))
	| (SUCC nat1, SUCC nat2) -> SUCC (SUCC (natadd (nat1, nat2)))

let rec natmul (a, b) =
	match (a, b) with
	| (ZERO, _) -> ZERO
	| (SUCC nat, _) -> (natadd (b, (natmul (nat, b))))

