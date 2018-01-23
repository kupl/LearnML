type nat = ZERO | SUCC of nat 

let rec natadd (n1,n2) =
	match n2 with
		ZERO -> n1
		| SUCC n22 -> natadd (SUCC n1,n22)

let natmul (n1,n2) =
	let rec helpmul n1 n2 add=
		match n2 with
			ZERO -> add
			| SUCC n22 -> helpmul n1 n22 (natadd (add,n1)) in
	helpmul n1 n2 ZERO


