type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
	match (a, b) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, b) -> b
	| (a, ZERO) -> a
	| (a, SUCC (c)) -> natadd (SUCC (a), c)

let rec natmul (a, b) = 
	match (a, b) with
	| (SUCC(ZERO), SUCC(ZERO)) -> SUCC(ZERO)
	| (a, SUCC(ZERO)) -> a
	| (SUCC(ZERO), b) -> b
	| (ZERO, ZERO) -> ZERO
	| (a, ZERO) -> ZERO
	| (ZERO, b) -> ZERO
	| (a, SUCC(c)) -> natadd (a, natmul(a, c))

