type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat =
 fun n1 n2 -> match n2 with ZERO -> n1 | SUCC n3 -> SUCC (natadd n1 n3)


let rec subnatmul n1 n2 n3 =
  match n1 with
  | ZERO -> n2
  | SUCC ZERO -> natadd n2 n3
  | SUCC n4 -> subnatmul n4 n2 (natadd n2 n3)


let natmul : nat -> nat -> nat = fun n1 n2 -> subnatmul n1 n2 ZERO
