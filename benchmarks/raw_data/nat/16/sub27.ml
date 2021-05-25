type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
match n1 with
ZERO ->  n2
| SUCC ZERO -> SUCC n2 
|SUCC (SUCC n) -> SUCC (SUCC (natadd n n2));;(* TODO *)
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
match n1 with
ZERO -> ZERO
| SUCC ZERO -> n2
| SUCC n -> natadd n2 (natmul n n2);;  (* TODO *)
