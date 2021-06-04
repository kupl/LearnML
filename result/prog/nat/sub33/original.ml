type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC n1_next -> natadd n1_next n2


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n2 with ZERO -> ZERO | SUCC n2_next -> natadd n1 (natmul n1 n2_next)
