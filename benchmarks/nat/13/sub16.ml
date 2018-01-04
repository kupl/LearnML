type nat = ZERO | SUCC of nat

let rec natadd(a, b) = 
	match b with
	| ZERO -> a
	| SUCC x -> SUCC(natadd (a, x))

let rec natmul(a, b) =
	match b with
	| ZERO -> ZERO
	| SUCC x -> natadd(a, natmul(a, x))
