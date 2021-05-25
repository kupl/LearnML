type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with ZERO -> n2
  | SUCC a -> natadd a (SUCC n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with ZERO -> ZERO
  | SUCC a -> natadd n2 (natmul a n2);;
