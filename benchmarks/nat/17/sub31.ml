type nat = ZERO | SUCC of nat
let natadd ((n1: nat), (n2: nat)) =
  let rec op a b =
    match (a, b) with
    | (ZERO, ZERO) -> ZERO
    | (ZERO, SUCC b') -> b
    | (SUCC a', ZERO) -> a
    | (SUCC a', SUCC b') -> SUCC (SUCC (op a' b'))
  in
  op n1 n2

let rec natmul ((n1: nat), (n2: nat)) =
  let op a b =
    match (a, b) with
    | (ZERO, ZERO) -> ZERO
    | (ZERO, SUCC b') -> ZERO
    | (SUCC a', ZERO) -> ZERO
    | (SUCC a', SUCC b') -> natadd (b, natmul (a', b))
  in
  op n1 n2