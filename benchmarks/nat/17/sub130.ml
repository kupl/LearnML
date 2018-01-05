type nat = ZERO | SUCC of nat

let rec natadd ((x : nat), (y : nat)) : nat =
  match x with
  | ZERO -> y
  | SUCC xMinus -> natadd (xMinus, SUCC y)

let rec natmul ((x : nat), (y : nat)) : nat =
  if x = ZERO || y = ZERO then ZERO
  else
    let rec consecadd n y res =
      match n with
      | ZERO -> res
      | SUCC nMinus -> consecadd nMinus y (natadd (y, res)) in
    consecadd x y ZERO
