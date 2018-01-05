(*
  CSE/2015-21233/김종권
  Homework 1-5
*)
type nat = ZERO | SUCC of nat

let rec natadd' (n1, n2) acc =
  match n1, n2 with
  | ZERO, ZERO -> acc
  | SUCC n, _ -> natadd' (n, n2) (SUCC acc)
  | _, SUCC n -> natadd' (n1, n) (SUCC acc)

let natadd (n1, n2) =
  natadd' (n1, n2) ZERO

let rec natmul' (n1, n2) acc =
  match n1 with
  | ZERO -> acc
  | SUCC z ->
    natmul' (z, n2) (natadd (n2, acc))

let natmul (n1, n2) =
  natmul' (n1, n2) ZERO
