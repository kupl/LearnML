type nat = ZERO | SUCC of nat

let rec func (m : nat) (n : int) : nat =
  if n = 1 then SUCC m else func (SUCC m) (n - 1)


let rec plus (a : nat) : int = match a with ZERO -> 0 | SUCC a' -> 1 + plus a'

let natadd (n1 : nat) (n2 : nat) : nat = func ZERO (plus n1 + plus n2)

let natmul (n1 : nat) (n2 : nat) : nat = func ZERO (plus n1 * plus n2)
