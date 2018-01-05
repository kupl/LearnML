type nat = ZERO | SUCC of nat

let rec natadd (a, b) = 
	match a with
	| ZERO -> b
	| SUCC pred -> SUCC (natadd (pred, b))

let rec natmul (a, b) =
	match a with
	| ZERO -> ZERO
	| SUCC pred -> natadd(natmul(pred, b), b)
