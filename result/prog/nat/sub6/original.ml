type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC nat -> natadd nat (SUCC n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n1 with
  | ZERO -> ZERO
  | SUCC nat -> if nat = ZERO then n2 else natmul nat (natadd n2 n2)
