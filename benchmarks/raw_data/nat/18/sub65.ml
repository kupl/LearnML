type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun a b->
match b with
| ZERO -> a
| SUCC n -> natadd (SUCC a) n;;

let rec natmul : nat -> nat -> nat
= fun a b->
match b with
| ZERO -> ZERO
| SUCC n -> natadd a (natmul a n);;