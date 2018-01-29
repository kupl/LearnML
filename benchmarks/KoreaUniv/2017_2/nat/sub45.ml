(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
		| ZERO -> n2
		| SUCC tail -> natadd tail (SUCC n2)

let natmul : nat -> nat -> nat 
= fun n1 n2 -> let rec loop n1 n2 i = 
		match i with
		| ZERO -> ZERO
		| SUCC tail -> natadd n2 (loop n1 n2 tail)
		in loop n1 n2 n1

let natexp : nat -> nat -> nat 
= fun n1 n2 -> let rec loop n1 n2 i = 
		match i with
		| ZERO -> SUCC ZERO
		| SUCC tail -> natmul n1 (loop n1 n2 tail)
		in loop n1 n2 n2

