type nat = ZERO | SUCC of nat

let rec natadd (a : nat) (b : nat) : nat =
  match b with ZERO -> a | SUCC x -> SUCC (natadd a x)


let rec natmul (a : nat) (b : nat) : nat =
  if a = ZERO || b = ZERO then ZERO else natadd b b
