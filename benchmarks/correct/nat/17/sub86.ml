
(*problem 2*) 
type nat = ZERO | SUCC of nat
 
let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
match n2 with
| ZERO -> n1
| SUCC n2_decrease -> natadd (SUCC n1) n2_decrease
 
let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
match n2 with
| ZERO -> ZERO
| SUCC n2_decrease -> natadd n1 (natmul n1 n2_decrease) 
 
let rec natexp : nat -> nat -> nat
= fun n1 n2 ->
match n2 with
| ZERO -> SUCC ZERO
| SUCC n2_decrease -> natmul n1 (natexp n1 n2_decrease)
 