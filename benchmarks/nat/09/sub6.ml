type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
	match a with
	ZERO -> b
	| SUCC am ->
		match b with
		ZERO -> a
		| SUCC bm -> SUCC(SUCC(natadd(am, bm)))

let rec natmul (a, b) =
	match a with
	ZERO -> ZERO
	| SUCC am ->
		match b with
		ZERO -> ZERO
		| SUCC bm -> natadd(natadd(a, ZERO),(natmul(a, bm)))
