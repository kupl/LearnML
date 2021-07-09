type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n2 with ZERO -> n1 | SUCC n3 -> SUCC (natadd n1 n3)


let rec subnatmul (n1 : nat) (n2 : nat) (n3 : nat) : nat =
  match n1 with
  | ZERO -> n2
  | SUCC ZERO -> natadd n2 n3
  | SUCC n4 -> subnatmul n4 n2 (natadd n2 n3)


let natmul (n1 : nat) (n2 : nat) : nat = subnatmul n1 n2 ZERO
