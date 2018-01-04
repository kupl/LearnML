type nat = ZERO | SUCC of nat
let rec natadd (a, b) =
	match a with
	ZERO -> b
	|SUCC c -> natadd (c, SUCC (b))

let rec natmul (a,b) =
	match a with
	ZERO -> ZERO
	|SUCC ZERO -> b
	|SUCC c -> natadd ((natmul (c, b)), b) 
