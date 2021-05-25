type nat =
	| ZERO
	| SUCC of nat

let rec natadd n1 n2 = 
	match n2 with
	| ZERO -> n1
	| SUCC ZERO -> SUCC n1
	| SUCC t -> SUCC (natadd n1 t) 

let rec natmul n1 n2 = 
	match n2 with
	| ZERO -> ZERO
	| SUCC ZERO -> n1
	| SUCC t -> natadd (natmul n1 t) n1
