type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match n1 with
  |ZERO->n2
    |SUCC(formula)->SUCC(natadd formula n2);;
   

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match n1 with
  |ZERO->ZERO
  |SUCC ZERO->n2
  |SUCC(formula)->natadd (natmul formula n2) n2;;
  

