type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with 
| ZERO -> n1
| SUCC(a) -> natadd (SUCC n1)  a

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match (n1, n2) with 
| (ZERO, b) -> ZERO
| (a, ZERO) -> ZERO
| (SUCC(ZERO), b) -> n2
| (a, SUCC(ZERO)) -> n1
| (a, SUCC(b)) -> natadd a (natmul a b)
