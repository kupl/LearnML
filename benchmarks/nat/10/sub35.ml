type nat = ZERO | SUCC of nat
let rec natadd (a, b) =
	match b with
	ZERO->a
	|SUCC(b_)-> natadd(SUCC(a),b_)
let rec natmul (a, b) =
	match b with
	ZERO->ZERO
	|SUCC(b_)-> (natadd(a,natmul(a,b_)))
