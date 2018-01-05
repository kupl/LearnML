(*2009-11718 1-7*)

type nat = ZERO | SUCC of nat

let rec natadd (n,m)=
	match (n,m) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC y) -> SUCC (natadd (ZERO, y))
	| (SUCC x, ZERO) -> SUCC (natadd (x, ZERO))
	| (SUCC x, SUCC y) -> SUCC (SUCC (natadd (x,y)))

let rec natmul (n,m)=
	match (n,m) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC y) -> ZERO
	| (SUCC x, ZERO) -> ZERO
	| (SUCC x, SUCC y) -> (match x with
						| ZERO -> natadd (ZERO, SUCC y)
						| _ -> natadd(SUCC y, natmul(x, SUCC y)))
