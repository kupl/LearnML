type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC n1 -> SUCC (natadd n1 n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> ZERO | SUCC n1 -> SUCC (natadd n2 (natmul n1 n2))
