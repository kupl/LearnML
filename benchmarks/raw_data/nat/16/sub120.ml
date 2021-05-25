type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
 | ZERO -> n2
 | SUCC(n) -> SUCC(natadd n n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let rec fun2 a b c = 
match a with 
 | ZERO -> c
 | SUCC(aa) -> fun2 aa b (natadd b c) in fun2 n1 n2 ZERO

