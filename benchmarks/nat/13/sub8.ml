type nat = 
    ZERO
  | SUCC of nat

let rec natadd (a, b) = 
  match (a, b) with
  | (ZERO, ZERO) -> ZERO
  | (ZERO, c) -> c
  | (c, ZERO) -> c
  | (SUCC c, SUCC d) -> SUCC (SUCC (natadd (c, d)))

let rec natmul (a, b) =
  match (a, b) with
  | (ZERO, ZERO) -> ZERO
  | (ZERO, _) -> ZERO
  | (_, ZERO) -> ZERO
  | (c, SUCC d) -> natadd (c, natmul(c, d))
