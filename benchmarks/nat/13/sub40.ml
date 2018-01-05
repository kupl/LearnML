type nat = ZERO | SUCC of nat

let rec natadd (n, m) =
  match m with
  | ZERO -> n
  | SUCC _m -> natadd (SUCC n, _m)

let rec natmul (n, m) =
  match m with
  | ZERO -> ZERO
  | SUCC _m -> natadd (n, natmul(n, _m))
