type nat = ZERO
         | SUCC of nat

let rec natadd (l, r) =
	match r with
	| ZERO -> l
	| SUCC rem -> natadd ((SUCC(l)), rem)

let rec natmul (l, r) =
	match r with
	| ZERO -> ZERO
	| SUCC rem -> natadd ((natmul (l, rem)), l)
