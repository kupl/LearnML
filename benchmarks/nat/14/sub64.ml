type nat = ZERO | SUCC of nat

let su n = SUCC n

let de n = 
	match n with
		| ZERO -> ZERO
		| SUCC natu -> natu

let rec natadd (n1, n2) = 
	match n2 with
		| ZERO -> n1
		| _ -> SUCC (natadd(n1, (de n2)))

let rec natmul (n1, n2) = 
	match n2 with
		| ZERO -> ZERO
		| _ -> natadd((natmul(n1, (de n2))), n1)


