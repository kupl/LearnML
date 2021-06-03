type nat = ZERO | SUCC of nat 
let rec natadd a b =
	match a with	
	ZERO -> b
	| SUCC l -> SUCC (natadd l b)
let rec natmul a b =
	match a with
	ZERO -> ZERO
	| (SUCC ZERO )-> b
	| SUCC l -> natadd b (natmul l b)