type nat = ZERO | SUCC of nat

let rec natadd(a, b) =
	match (a, b) with
	| (x, SUCC y) -> natadd(SUCC x, y)
	| (x, ZERO) -> x
let rec natmul(a, b) =
	match (a, b) with
	| (x, SUCC y) -> natadd(x, natmul(x, y))
	| (x, ZERO) -> ZERO