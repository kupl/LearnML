

type nat = ZERO | SUCC of nat

let rec natadd (x, y) =
	match y with
	| ZERO -> x
	| SUCC a -> natadd(SUCC x, a)

let rec natmul (x, y) =
	match y with
	| ZERO -> ZERO
	| SUCC a -> natadd(natmul(x, a), x)


