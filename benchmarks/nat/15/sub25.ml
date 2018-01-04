type nat = ZERO
	 | SUCC of nat
   
let rec natadd (x: nat) (y: nat): nat =
  match x with
  | ZERO -> y
  | SUCC x' -> SUCC (natadd x' y) 

let rec natmul (x: nat) (y: nat): nat =
  match x with
  | ZERO -> ZERO
  | SUCC x' -> natadd (natmul x' y) y

