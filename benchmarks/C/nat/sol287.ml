type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
  | ZERO->n2
  | SUCC ln1->natadd ln1 (SUCC n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  | ZERO->ZERO
  | SUCC ln1->natadd n2 (natmul ln1 n2);;

  

