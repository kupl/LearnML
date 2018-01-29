(* problem 2*) 
type nat = ZERO 
	| SUCC of nat 

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
match n2 with
	|ZERO -> n1
	|SUCC m -> natadd (SUCC n1) m

(* TODO *) 
let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
match n2 with
 	|ZERO -> ZERO
 	|SUCC m -> natadd n1 (natmul n1 m)
(* TODO *) 
let natexp : nat -> nat -> nat 
= fun n1 n2 -> 
match n2 with
	|ZERO -> SUCC ZERO
	|SUCC m -> if m = ZERO then SUCC ZERO else  natmul n1 (natmul n1 m) 
