type nat =
	| ZERO
	| SUCC of nat

let rec natadd n1 n2 =
	match n1 with
	| ZERO -> n2
	| SUCC n3 -> SUCC (natadd n3 n2);;

let rec natmul n1 n2 = 
	match n2 with
	| ZERO -> ZERO
	| SUCC n3 -> natadd n1 (natmul n1 n3);; 
