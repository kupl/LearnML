type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with
  | ZERO -> n2
  | SUCC a -> if a = ZERO then SUCC n2 else SUCC (natadd a n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  if n1 = ZERO then ZERO
  else match n1 with ZERO -> ZERO | SUCC __s11 -> natadd n2 (natmul n2 __s11)
