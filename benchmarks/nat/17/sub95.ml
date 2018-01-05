type nat = ZERO | SUCC of nat

let rec natadd (x, y) =
  match x, y with
  | (ZERO, _) -> y
  | (_, ZERO) -> x
  | (SUCC(i), SUCC(j)) -> SUCC ( SUCC (natadd (i, j)) )

let rec natmul (x, y) =
  match x, y with
  | (ZERO, _) -> ZERO
  | (_, ZERO) -> ZERO
  | (SUCC(i), SUCC(j)) -> natadd (x, natmul(x, j))
