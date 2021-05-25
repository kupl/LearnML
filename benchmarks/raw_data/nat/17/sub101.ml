(* problem 2*)
type nat = ZERO | SUCC of nat
;;

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC n1_two -> SUCC (natadd n1_two n2)
;;

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC n1_two -> natadd n2 (natmul n1_two n2)
;;