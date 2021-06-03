(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
		| SUCC n -> SUCC (natadd n n2)
		| ZERO -> n2

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
		| SUCC n -> natadd n2 (natmul n n2)
		| ZERO -> ZERO

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
		| SUCC n -> natmul n1 (natexp n1 n)
		| ZERO -> (SUCC ZERO)

