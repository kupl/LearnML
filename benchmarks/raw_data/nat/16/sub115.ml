type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n1 with 
	| ZERO -> n2
	| SUCC n11 -> SUCC ( natadd n11 n2 );;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	match n1 with
	| ZERO -> ZERO
	| SUCC n11 -> natadd ( natmul n11 n2 ) n2;;
