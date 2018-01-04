type nat = ZERO | SUCC of nat
let rec natadd(n1, n2) =
	match n1 with
	| ZERO -> n2
	| SUCC nn -> natadd(nn,(SUCC(n2)))
let rec natmul(n1, n2) =
	match n1 with
	| ZERO -> ZERO
	| SUCC nn -> natadd(n2,(natmul(nn,n2)))