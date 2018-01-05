type nat = ZERO | SUCC of nat

let rec evalnat n =
	match n with
	ZERO -> 0
	| SUCC n1 -> 1+(evalnat n1)

let natadd (n1, n2) =
	(evalnat n1) + (evalnat n2)

let natmul (n1, n2) =
	(evalnat n1) * (evalnat n2)
