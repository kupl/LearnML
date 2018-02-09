type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
	| ZERO -> n1
	| SUCC (t) -> natadd (SUCC(n1)) t;;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
	| ZERO -> ZERO
	| SUCC ZERO -> n1
	| SUCC (t) -> natadd n1 (natmul n1 t);;
