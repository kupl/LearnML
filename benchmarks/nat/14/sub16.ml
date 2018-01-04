(*2009-11718 박준상 1-3*)

type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
	match (a, b) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC y) -> SUCC (natadd (ZERO, y))
	| (SUCC x, ZERO) -> SUCC (natadd (x, ZERO))
	| (SUCC x, SUCC y) -> SUCC (SUCC (natadd (x, y)))


let rec natmul (a, b) =
	match (a, b) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC y) -> ZERO
	| (SUCC x, ZERO) -> ZERO
	| (SUCC x, SUCC y) -> natadd (SUCC x, natmul(SUCC x, y))
