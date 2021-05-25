type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	| ZERO -> n2
	| SUCC(v) -> (natadd v (SUCC(n2)))

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	| ZERO -> ZERO
	| SUCC(v) -> (natadd n2 (natmul v n2))
