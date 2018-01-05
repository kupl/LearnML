type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
	match (a, b) with
	| (_, ZERO) -> a
	| (ZERO, _) -> b
	| (_, SUCC rb) -> natadd (SUCC a, rb)

let rec natmul (a, b) =
	match (a, b) with
	| (_, ZERO) -> ZERO
	| (ZERO, _) -> ZERO
	| (_, SUCC ZERO) -> a
	| (_, SUCC rb) -> natadd (a, natmul (a, rb))
