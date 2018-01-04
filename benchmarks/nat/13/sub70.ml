type nat = ZERO | SUCC of nat

let rec natadd (a, b) = match (a, b) with
  | (a, ZERO) -> a
  | (ZERO, b) -> b
  | (a, SUCC(b)) -> SUCC(natadd(a, b))
let rec natmul (a, b) = match (a, b) with
  | (a, ZERO) -> ZERO
  | (ZERO, b) -> ZERO
  | ((SUCC ZERO), b) -> b
  | (a, (SUCC ZERO)) -> a
  | (a, SUCC(b)) -> natadd(a, natmul(a, b))
