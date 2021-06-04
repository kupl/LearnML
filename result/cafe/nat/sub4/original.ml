type nat = ZERO | SUCC of nat

let rec natadd (nat1 : nat) (nat2 : nat) : nat =
  match nat1 with ZERO -> nat2 | SUCC nat -> natadd nat (SUCC nat2)


let rec natmul (nat1 : nat) (nat2 : nat) : nat =
  match nat1 with ZERO -> ZERO | SUCC nat -> natmul nat (natadd nat2 nat2)
