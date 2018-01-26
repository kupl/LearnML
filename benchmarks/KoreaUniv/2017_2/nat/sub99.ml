(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
ZERO -> n1
| SUCC n2 -> SUCC (natadd n1 n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
ZERO -> ZERO
| SUCC n2 -> natadd n1 (natmul n1 n2)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
ZERO -> SUCC ZERO
| SUCC n2 -> natmul n1 (natexp n1 n2);;