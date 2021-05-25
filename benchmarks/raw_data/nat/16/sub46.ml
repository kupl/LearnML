type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC n_1 -> natadd n_1 (SUCC n2)  

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC n_1 ->  natadd n2 (natmul n_1 n2) 
