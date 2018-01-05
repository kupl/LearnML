type nat = ZERO | SUCC of nat

let rec natadd (x, y) =
	match x with
	ZERO -> y
	| SUCC(_nat) -> natadd(_nat, SUCC(y))

let rec natmul (x, y) =
	match x with
	ZERO -> ZERO
	| SUCC(ZERO) -> y
	| SUCC(_nat) -> natadd(y, natmul(_nat, y))
