type nat = ZERO | SUCC of nat

let rec natadd (a, b) = 
  match b with
  | ZERO -> a
  | SUCC b1 -> natadd (SUCC a, b1)

let rec natmul (a, b) =
  match b with
  | ZERO -> ZERO
  | SUCC b1 -> natadd(a, natmul(a, b1))
