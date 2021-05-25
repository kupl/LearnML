type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	match n2 with
	| SUCC a -> SUCC (natadd n1 a)
	| ZERO -> n1

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	match n2 with
	| SUCC a -> if a = ZERO then n1 else natadd n1 (natmul n1 a)
	| ZERO -> ZERO
