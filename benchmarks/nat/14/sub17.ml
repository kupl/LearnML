(* ex3 *)
type nat = ZERO | SUCC of nat

let rec natadd (x, y) =
  match y with
  | ZERO -> x
  | SUCC n -> natadd (SUCC x, n)

let rec natmul (x, y) =
  match y with
  | ZERO -> ZERO
  | SUCC n -> natadd (natmul (x, n), x)
