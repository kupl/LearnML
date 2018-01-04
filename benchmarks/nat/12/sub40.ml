type nat = ZERO 
		 | SUCC of nat

let rec natadd(a, b) = 
	match a with 
	| ZERO -> b
	| SUCC nata -> SUCC(natadd(nata, b))

let rec natmul(a, b) =
	match a with 
	| ZERO -> ZERO
	| SUCC nata -> natadd(b, natmul(nata, b))
