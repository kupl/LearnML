type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
	match n1, n2 with
	ZERO, ZERO -> ZERO
	| ZERO, _ -> n2
	| _, ZERO -> n1
	|_, SUCC(n) -> natadd(SUCC(n1), n)

let rec natmul (n1, n2) =
	match n1, n2 with
	ZERO, _ -> ZERO
	| _, ZERO -> ZERO
	| SUCC(ZERO), _ -> n2
	| _, SUCC(ZERO) -> n1
	| _, SUCC(n) -> natadd(n1, natmul(n1, n))
