type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    | ZERO -> n2
    | SUCC n1_pred -> SUCC (natadd n1_pred n2);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    | ZERO -> ZERO
    | SUCC n1_pred -> natadd n2 (natmul n1_pred n2);;


