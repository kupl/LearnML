type nat = ZERO | SUCC of nat

let rec natadd (x,y) =
  match x with
  | ZERO -> y
  | SUCC pred -> natadd(pred, SUCC y)

let rec natmul (x,y) =
  match y with
  | ZERO -> ZERO
  | SUCC pred -> natadd (natmul(x, pred),x)