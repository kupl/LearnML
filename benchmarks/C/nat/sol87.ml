type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)
match n1 with
|ZERO -> n2
|SUCC m -> natadd m (SUCC n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)
match n1 with
|ZERO -> ZERO
|SUCC m -> if m= ZERO then n2 else natadd (natmul n2 m) n2
