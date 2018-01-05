type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) = 
	match n1 with
	| ZERO ->
		(match n2 with
		| ZERO -> ZERO
		| SUCC (n22) -> SUCC (natadd (n1, n22))
		)
	| SUCC (n11) ->
		SUCC (natadd (n11, n2))

let rec natmul (n1, n2) =
	match n1 with
	| ZERO -> ZERO
	| SUCC (n11) -> natadd ((natmul (n11, n2)), n2)
