
type nat = 	ZERO | SUCC of nat

let rec natadd(a,b) =
	match (a,b) with
	|(ZERO, b) -> b
	|(a, ZERO) -> a
	|((SUCC a),b) -> SUCC (natadd(a,b))

let rec natmul(a,b) =
	match (a,b) with
	|(ZERO, b) -> ZERO
	|(a, ZERO) -> ZERO
	|((SUCC a), b) -> natadd(b, natmul(a,b)) 


