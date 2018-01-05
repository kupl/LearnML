type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
  match a with
  | ZERO -> b
  | SUCC a -> SUCC (natadd (a, b))

let rec natmul (a, b) =
  match a with
  | ZERO -> ZERO
  | SUCC a -> natadd (b, natmul(a, b))
