type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
  match (a, b) with
  | (ZERO, ZERO) -> ZERO
  | (ZERO, b) -> b
  | (SUCC c, b) -> natadd (c, (SUCC b))

let rec natmul (a, b) =
  match (a, b) with
  | (ZERO, b) -> ZERO
  | (a, ZERO) -> ZERO
  | (a, SUCC c) -> natadd (a, natmul(a, c))
