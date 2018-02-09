type nat = ZERO | SUCC of nat

let rec natadd a b =
	match b with
	| ZERO -> a
	| SUCC(x) -> SUCC(natadd a x)

let rec natmul a b =
	if a = ZERO || b = ZERO then ZERO
	else natadd b b 
