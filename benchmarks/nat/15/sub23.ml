type nat = ZERO | SUCC of nat

let rec natadd a b =
	match b with
	ZERO		-> a
	|SUCC(b_pred)	-> natadd (SUCC(a)) b_pred

let rec natmul a b = mul a b ZERO
and mul a b c =
	match b with
	ZERO		-> c
	|SUCC(b_pred)	-> mul a b_pred (natadd c a)
