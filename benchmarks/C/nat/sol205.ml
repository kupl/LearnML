(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with
 | ZERO -> n2
 | SUCC(t)->natadd (t) (SUCC(n2))

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with
 | ZERO -> ZERO
 | SUCC(ZERO) -> n2
 | SUCC(t) -> natadd (natmul t n2) n2

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
match n2 with
 | ZERO -> SUCC(ZERO)
 | SUCC(ZERO) -> n1
 | SUCC(t) -> natmul n1 (natexp n1 t)