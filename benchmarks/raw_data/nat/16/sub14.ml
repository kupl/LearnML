type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
	match n2 with
	| ZERO -> n1
	| SUCC (n) -> SUCC (natadd n1 n)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
	match n1,n2 with
	| ZERO,_ -> ZERO
	| _,ZERO -> ZERO
	| _,SUCC(ZERO) -> n1
	| SUCC(ZERO),_ -> n2
	| _,SUCC(n) -> natadd (natmul n1 n) n1 

