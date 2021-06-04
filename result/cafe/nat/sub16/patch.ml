type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n2 with ZERO -> n1 | SUCC k2 -> SUCC (natadd n1 k2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  if n1 = ZERO then ZERO
  else match n1 with ZERO -> ZERO | SUCC __s11 -> natadd n2 (natmul n2 __s11)


let rec natexp (n1 : nat) (n2 : nat) : nat =
  match n2 with
  | ZERO -> SUCC ZERO
  | SUCC k -> if k = ZERO then SUCC ZERO else natmul (natmul n1 k) n1
