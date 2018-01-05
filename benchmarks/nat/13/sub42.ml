type nat = ZERO | SUCC of nat

let rec natadd = fun pair ->
	match pair with
		(ZERO, a) -> a
		|(SUCC(n), a) -> natadd (n, SUCC(a))

let rec natmul = fun pair ->
	match pair with
		(ZERO, a) -> ZERO
		|(SUCC(n), a) -> natadd (a, natmul(n,a))
