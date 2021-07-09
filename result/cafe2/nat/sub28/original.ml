type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with SUCC temp -> SUCC (natadd temp n2) | ZERO -> n2


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n1 with SUCC temp -> natmul temp (natadd n1 n2) | ZERO -> n2
