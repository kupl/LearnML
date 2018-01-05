type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
	match (a, b) with
		| (ZERO, _) -> b
		| (_, ZERO) -> a
		| (SUCC prev,_) -> natadd (prev, SUCC b)
let rec natmul (a, b) =
	match (a, b) with
		| (ZERO, _) -> a
		| (_, ZERO) -> b
		| (SUCC prev, _) -> natadd (b, natmul(prev, b))
