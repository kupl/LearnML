type nat = ZERO | SUCC of nat

let rec func m n = if n = 1 then SUCC m else func (SUCC m) (n - 1)

let rec plus a = match a with ZERO -> 0 | SUCC a' -> 1 + plus a'

let natadd : nat -> nat -> nat = fun n1 n2 -> func ZERO (plus n1 + plus n2)

let natmul : nat -> nat -> nat = fun n1 n2 -> func ZERO (plus n1 * plus n2)
