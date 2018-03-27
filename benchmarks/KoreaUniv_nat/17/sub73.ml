(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with 
|ZERO -> n2
|SUCC n1_mi -> SUCC (natadd n1_mi n2);;

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with
|ZERO -> ZERO
|SUCC n1_mi -> natadd n2 (natmul n1_mi n2);;

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
match n2 with
|ZERO -> SUCC ZERO
|SUCC n2_minus_1 -> natmul n1 (natexp n1 n2_minus_1);;
