type nat = ZERO | SUCC of nat

let rec natadd (x, y) : nat =
  match x, y with
  | ZERO, _ -> y
  | _, ZERO -> x
  | SUCC a', _ -> SUCC (natadd (a', y))

let rec natmul (x, y) : nat =
  match x, y with
  | ZERO, _ -> ZERO
  | _, ZERO -> ZERO
  | SUCC ZERO, _ -> y
  | _, SUCC ZERO -> x
  | SUCC n', _ -> natadd(y, natmul (n', y))
    
