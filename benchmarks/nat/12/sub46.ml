type nat =
	ZERO
	| SUCC of nat

let rec natadd (nat1, nat2) =
	match (nat1, nat2) with
	(_, ZERO) -> nat1
	| (_, SUCC nat) -> natadd (SUCC nat1, nat) 

let rec natmul (nat1, nat2) =
	match (nat1, nat2) with
	(_, ZERO) -> ZERO
	| (_, SUCC nat ) -> natadd ( nat1, natmul (nat1, nat) )
