type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  | ZERO -> n2
  | SUCC n -> SUCC (natadd n n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  | ZERO -> ZERO
  | SUCC ZERO -> n2
  | SUCC n -> if n2 = ZERO then ZERO else if n2 = SUCC ZERO then n1 else natadd (natmul n n2) n2;;
