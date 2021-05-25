type nat = ZERO | SUCC of nat;;

let rec natadd a b =
	match a with
	ZERO -> b
	| SUCC (x) -> natadd x (SUCC(b));;

let rec natmul a b =
	match a with
	ZERO -> ZERO
	| SUCC ZERO -> b
	| SUCC (x) -> natadd (natmul x b) b;;