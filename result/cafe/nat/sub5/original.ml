type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match (n1, n2) with
  | ZERO, ZERO -> ZERO
  | SUCC n1', ZERO -> SUCC (natadd n1' n2)
  | ZERO, SUCC n2' -> SUCC (natadd n1 n2')
  | SUCC n1', SUCC n2' -> SUCC (natadd n1' n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match (n1, n2) with
  | ZERO, ZERO -> ZERO
  | SUCC n1', ZERO -> SUCC (natmul n1' n2)
  | ZERO, SUCC n2' -> SUCC (natmul n1 n2')
  | SUCC n1', SUCC n2' -> SUCC (natmul n1' n2)
