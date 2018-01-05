type nat = ZERO | SUCC of nat

let rec natadd(a,b) = 
	match(b) with
		ZERO -> a
		| SUCC(n) -> natadd(SUCC(a),n)

let rec natmul(a,b) = 
	match(b) with
		ZERO -> ZERO
		| SUCC(n) -> natadd(natmul(a,n),a)


