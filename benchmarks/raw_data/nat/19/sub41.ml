type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n2 with
    |ZERO->n1
    |SUCC bef -> natadd (SUCC n1) bef

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
  match n2 with
    |ZERO -> ZERO
    |SUCC bef-> natadd n1 (natmul n1 bef);;


