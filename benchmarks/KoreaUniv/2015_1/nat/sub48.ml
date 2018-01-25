type nat = ZERO | SUCC of nat
let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
	match n1, n2 with
	| ZERO, ZERO -> ZERO
	| SUCC (n1'), ZERO -> SUCC (natadd n1' n2)
	| ZERO, SUCC (n2') -> SUCC (natadd n1 n2')
	| SUCC (n1'), SUCC (n2') -> SUCC (natadd n1' n2)
	

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
	match n1, n2 with
	| ZERO, ZERO -> ZERO
	| SUCC (n1'), ZERO -> SUCC (natmul n1' n2)
	| ZERO, SUCC (n2') -> SUCC (natmul n1 n2')
	| SUCC (n1'), SUCC (n2') -> SUCC (natmul n1' n2)