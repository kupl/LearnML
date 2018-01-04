type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
	match a with
		| ZERO -> b
		| SUCC a' -> SUCC (natadd (a',b))

let rec natmul (c, d) =
	match c with
		| ZERO -> ZERO
		| SUCC c' -> natadd(natmul(c', d), d)