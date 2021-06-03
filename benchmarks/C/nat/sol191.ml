(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
	match n2 with
	|ZERO -> n1
	|SUCC i -> SUCC (natadd n1 i)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
	match n2 with
	|ZERO -> ZERO
	|SUCC i -> natadd n1 (natmul n1 i)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> 
	match n2 with
	|ZERO -> SUCC ZERO
	|SUCC i -> natmul n1 (natexp n1 i)
