type nat = ZERO | SUCC of nat

let rec natadd ((a:nat), (b:nat)) = match a with
	| ZERO -> b
	| SUCC (a_succ) -> SUCC (natadd (a_succ, b))

let rec natmul ((a:nat), (b:nat)) = match a with
	| ZERO -> ZERO
	| SUCC (a_succ) -> natadd (natmul (a_succ, b), b)
