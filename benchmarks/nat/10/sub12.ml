type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) = 
	match n2 with
	ZERO -> n1
	|SUCC n -> natadd (SUCC n1, n)

let rec natmul (n1, n2) = 
	natmul2 (n1, n1, n2) 
	and natmul2 (n1, added, n2) = 
		match n2 with
		| ZERO -> ZERO
		| SUCC ZERO -> n1
		| SUCC n -> natmul2 (natadd(n1, added), added, n)
