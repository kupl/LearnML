type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
  match (a, b) with
  |(ZERO, ZERO) -> ZERO
  |(v, ZERO) -> v
  |(ZERO, v) -> v
  |(_, SUCC v) -> natadd(SUCC a, v)

let rec natmul (a, b) =
  match (a, b) with
  |(_, ZERO) -> ZERO
  |(ZERO, _) -> ZERO
  |(_, SUCC v) -> natadd(a, natmul(a, v))
