type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
(* TODO *)
	match n2 with
	|ZERO -> n1
	|(SUCC p) -> if p == ZERO then (SUCC n1) else natadd (SUCC n1) p

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
	match n2 with
	|ZERO -> ZERO
	|(SUCC p) -> if p == ZERO then n1 else natadd (natmul n1 p) n1
