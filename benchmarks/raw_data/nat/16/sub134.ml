type nat =
	| ZERO
	| SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
	| ZERO -> n1
	| SUCC(a) -> natadd (SUCC(n1)) a;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	match n2 with
	| ZERO -> ZERO
	| SUCC(a) -> natadd n1 (natmul n1 a);;
