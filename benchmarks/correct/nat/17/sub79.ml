
(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->

match n1 with
| ZERO -> n2
| SUCC n1m1 -> SUCC (natadd n1m1 n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with
| ZERO -> ZERO
| SUCC n1m1 -> natadd n2 (natmul n1m1 n2)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
match n2 with
| ZERO -> SUCC ZERO
| SUCC n2m1 -> natmul n1 (natexp n1 n2m1)
