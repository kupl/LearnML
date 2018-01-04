(* 2006-11377 hw1-3 *)

type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) =
	match (nat1, nat2) with
	| (ZERO, _) -> nat2
	| (_, ZERO) -> nat1
	| (_, SUCC oneless) -> natadd (SUCC nat1, oneless)
	
let rec natmul (nat1, nat2) =
	match (nat1, nat2) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (_, SUCC oneless) -> natadd (nat1, natmul (nat1, oneless))
	