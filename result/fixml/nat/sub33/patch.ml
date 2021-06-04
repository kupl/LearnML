type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat =
 fun n1 n2 ->
  match n1 with ZERO -> n2 | SUCC n1_next -> natadd n1_next (SUCC n2)


let rec natmul : nat -> nat -> nat =
 fun n1 n2 ->
  match n2 with ZERO -> ZERO | SUCC n2_next -> natadd n1 (natmul n1 n2_next)
