type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with 
  ZERO->n1
  |SUCC(temp)->SUCC(natadd n1 temp);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
  ZERO->ZERO
  |SUCC(temp)->natadd (natmul n1 temp) n1;;
  
  
let two = SUCC(SUCC ZERO);;
let three = SUCC(SUCC(SUCC ZERO));;
  
natadd two three;;
natmul two three;;
