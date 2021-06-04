type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with
  | ZERO -> n2
  | SUCC a -> if a = ZERO then SUCC n2 else SUCC (natadd a n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n1 with
  | ZERO -> ZERO
  | SUCC a -> if a = ZERO then n2 else natadd (natmul n2 a) n2
