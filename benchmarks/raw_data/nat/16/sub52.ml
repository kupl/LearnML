type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
| ZERO -> n2
| SUCC nat_a -> SUCC(natadd nat_a n2)  

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC nat_a -> natadd (natmul nat_a n2) n2 
