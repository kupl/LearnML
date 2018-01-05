type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
	match n2 with
	| ZERO -> n1
	| SUCC n2_sub -> natadd ((SUCC n1), n2_sub)

let rec natmul (n1, n2) =
	match n2 with
	| ZERO -> ZERO
	| SUCC n2_sub -> natadd (n1, (natmul (n1, n2_sub)))
