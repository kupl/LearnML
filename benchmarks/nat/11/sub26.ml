type nat = ZERO | SUCC of nat

let rec natadd (a,b) =
	match a with
		| ZERO -> b
		| SUCC(k) -> natadd (k,SUCC(b))

let rec natmul (a,b) =
	match a with
		| ZERO -> ZERO
		| SUCC(k) -> natadd (natmul(k,b),b)
