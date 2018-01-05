type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) = 
		match n2 with 
		ZERO -> n1
		| SUCC n -> natadd (SUCC (n1), n) 

let natmul (n1, n2) =
		let rec natmul_sub (a, b, c)=
		match b with 
		ZERO -> c 
		| SUCC n -> natmul_sub (a, n, (natadd (a, c))) in
		natmul_sub (n1, n2, ZERO)
		
