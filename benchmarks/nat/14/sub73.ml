type nat = ZERO | SUCC of nat

let rec natadd (n0, n1) =
	match n1 with
	| ZERO -> n0
	| SUCC n -> natadd (SUCC n0, n)

let rec natmul (n0, n1) =
	match n1 with
	| ZERO -> ZERO
	| SUCC ZERO -> n0
	| SUCC n -> natadd (n0, natmul (n0, n))

