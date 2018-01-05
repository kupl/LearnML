type nat = ZERO | SUCC of nat

let rec natadd(a, b) = match a with
	| ZERO -> b
	| SUCC c -> (match b with
		| ZERO -> a
		| SUCC d -> natadd(c, SUCC(b))
	)

let rec _mul(a, b) m = match a with
	| ZERO -> ZERO
	| SUCC ZERO -> b
	| SUCC c -> (match b with
		| ZERO -> ZERO
		| SUCC ZERO -> a
		| SUCC d -> _mul(c, natadd(b, m)) m
	)

let natmul(a, b) = _mul(a, b) b
