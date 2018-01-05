type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) = 
	match (nat1, nat2) with
	| (ZERO, _) -> nat2
	| (_, ZERO) -> nat1
	| (_, SUCC right) -> natadd (SUCC nat1, right)



let rec natmul (nat1, nat2) = 
	match (nat1, nat2) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (_, SUCC ZERO) -> nat1
	| (_, SUCC right) -> natadd (nat1, natmul (nat1, right))
