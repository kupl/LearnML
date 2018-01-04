type nat = ZERO | SUCC of nat


let rec natadd (a,b) =
	match b with
	| ZERO -> a
	| SUCC b' ->  natadd ( (SUCC a), b')

let rec natmul (a, b) =
	match b with
	| ZERO -> ZERO
	| SUCC b' -> natadd( a, natmul(a, b'))
