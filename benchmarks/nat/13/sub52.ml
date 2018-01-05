type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) =
	match (nat1, nat2) with
	| (x, ZERO) -> x
	| (x, SUCC(y)) -> natadd(SUCC(x), y)

let rec natmul (nat1, nat2) =
	match (nat1, nat2) with
	| (x, ZERO) -> ZERO
	| (x, SUCC(y)) -> natadd(natmul(x, y), SUCC(y))