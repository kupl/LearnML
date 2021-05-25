type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
	match n1 with
	ZERO -> n2
	|SUCC (nat) -> SUCC (natadd nat n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
	match n1 with
	ZERO -> ZERO
	|SUCC (nat) -> natadd n2 (natmul nat n2);;