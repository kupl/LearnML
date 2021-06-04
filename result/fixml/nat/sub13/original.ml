type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat =
 fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC a -> if a = ZERO then SUCC n2 else SUCC (natadd a n2)


let rec natmul : nat -> nat -> nat =
 fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC a -> if a = ZERO then n2 else natmul a (natadd n2 n2)
