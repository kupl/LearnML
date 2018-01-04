type nat =
	| ZERO
	| SUCC of nat

let rec natadd (x, y) =
	match (x, y) with
	| (n, ZERO) -> n
	| (ZERO, n) -> n
	| (a, SUCC b) -> natadd (SUCC a, b)

let rec natmul (x, y) =
	match (x, y) with
	| (_, ZERO) -> ZERO
	| (ZERO, _) -> ZERO
	| (a, SUCC b) -> natadd (a, natmul (a, b))