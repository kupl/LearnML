type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	| ZERO -> n2
	| SUCC remaining -> natadd remaining (SUCC n2);; (*retval will represent the nat number 1 lower than n1, thus decrementing n1 by 1 while incrementing n2 by 1*)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
	| ZERO -> ZERO
	| SUCC remaining -> natadd n2 (natmul remaining n2);; (*Unsure if it is acceptable to use previous add function, but this makes the most sense to me*)
