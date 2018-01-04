type nat = ZERO | SUCC of nat

let rec natadd ((x : nat), (y : nat)) : nat =
  match x with
  | ZERO -> y
  | SUCC z  -> natadd(z, SUCC y)

let rec natmul ((x : nat), (y : nat)) : nat =
  match x with
  | ZERO -> ZERO
  | SUCC z -> natadd(y, natmul(z, y))
