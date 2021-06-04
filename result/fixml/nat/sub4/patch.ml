type nat = ZERO | SUCC of nat

let rec natadd nat1 nat2 =
  match nat1 with ZERO -> nat2 | SUCC nat -> natadd nat (SUCC nat2)


let rec natmul nat1 nat2 =
  match nat1 with ZERO -> ZERO | SUCC nat -> natadd nat2 (natmul nat2 nat)
