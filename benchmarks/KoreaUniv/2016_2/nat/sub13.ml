type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
SUCC( k ) -> natadd (SUCC (n1)) k 
| ZERO -> n1
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
SUCC( k ) -> natadd n1 (natmul n1 k)
| ZERO -> ZERO
