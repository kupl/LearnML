
(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
|SUCC(nat) -> SUCC(natadd nat n2)
|ZERO -> n2

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
|SUCC(nat) -> natadd n2 (natmul nat n2)
|ZERO -> ZERO

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
|SUCC(ZERO) -> n1
|ZERO -> ZERO
|SUCC(nat) -> natmul n1 (natexp n1 nat)

