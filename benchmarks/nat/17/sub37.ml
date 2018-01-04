type nat
  = ZERO
  | SUCC of nat

let rec natadd (n1, n2) =
  match n1, n2 with
  | ZERO, n2 -> n2
  | n1, ZERO -> n1
  | SUCC n1_pre, n2 -> SUCC (natadd (n1_pre, n2))

let rec natmul (n1, n2) =
  match n1, n2 with
  | ZERO, _ -> ZERO
  | _, ZERO -> ZERO
  | SUCC n1_pre, n2 -> natadd (n2, natmul (n1_pre, n2))
