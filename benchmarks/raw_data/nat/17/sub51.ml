type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
   match n2 with
   | ZERO -> n1
   | SUCC n3 -> natadd (SUCC n1) n3;;


let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n2 with
  | ZERO -> ZERO
  | SUCC ZERO -> n1
  | SUCC n3 -> natadd n1 (natmul n1 n3);;

let rec natexp : nat -> nat -> nat
=fun n1 n2 -> 
  match n2 with
  | ZERO -> SUCC ZERO
  | SUCC ZERO -> n1
  | SUCC n3 -> natmul n1 (natexp n1 n3);;
