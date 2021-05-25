type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	ZERO -> n2
	|SUCC(sub_n) -> SUCC(natadd sub_n n2) (* TODO *)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	ZERO -> ZERO
	|SUCC(sub_n) -> natadd (natmul sub_n n2) (n2) (* TODO *)
