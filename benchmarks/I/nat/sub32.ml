type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match n1 with
  | ZERO -> n2
  | SUCC nn -> natadd nn (SUCC n2)


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match n1 with
  | ZERO -> ZERO
  | SUCC ZERO -> n2
  | SUCC nn -> natmul nn (natadd n2 n2)
  
