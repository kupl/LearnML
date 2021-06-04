type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC n3 -> natadd n3 (SUCC n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n1 with
  | SUCC ZERO -> n2
  | ZERO -> ZERO
  | SUCC n3 -> natmul n3 (natadd n2 n2)
