type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC s -> natadd s (SUCC n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n1 with
  | ZERO -> ZERO
  | SUCC ZERO -> n2
  | SUCC s -> natmul s (natadd n2 n2)
