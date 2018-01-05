type nat = ZERO | SUCC of nat

let rec natadd ((n1: nat), (n2: nat)): nat = 
	if(n2 == ZERO) then n1
	else 
		match n1 with 
		| ZERO -> n2
		| SUCC n -> SUCC (natadd (n,n2))

let rec natmul ((n1: nat), (n2: nat)): nat =
	if (n2 == ZERO) then ZERO
	else if (n2 == SUCC ZERO) then n1
	else 
		match n1 with
		| ZERO -> ZERO
		| SUCC ZERO -> n2
		| SUCC n -> natadd (natmul (n, n2), n2)