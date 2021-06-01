type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  match n1 with
  | SUCC(temp)-> SUCC(natadd temp n2)
  | ZERO -> n2;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  match n1 with
  | SUCC(temp)-> natmul temp (natadd n1 n2)
  | ZERO -> n2;;