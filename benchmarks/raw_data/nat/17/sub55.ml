
(*Problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd nat1 nat2 = 
  match nat1 with
  |ZERO -> nat2
  |SUCC x -> natadd x (SUCC nat2)

let rec natmul nat1 nat2 = 
  match nat1 with
  |ZERO -> ZERO
  |SUCC ZERO -> nat2
  |SUCC x -> natadd nat2 (natmul x nat2)

let rec natexp nat1 nat2 = 
  match nat2 with
  |ZERO -> SUCC ZERO
  |SUCC x -> natmul nat1 (natexp nat1 x)