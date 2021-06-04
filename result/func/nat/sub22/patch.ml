type nat = ZERO | SUCC of nat

let rec func (m : nat) (n : int) : nat =
  if n = 1 then SUCC m else func (SUCC m) (n - 1)


let rec plus (a : nat) : int = match a with ZERO -> 0 | SUCC a' -> 1 + plus a'

let natadd (n1 : nat) (n2 : nat) : nat = func ZERO (plus n1 + plus n2)

let rec __s3 (__s4 : int) : nat =
  match __s4 with 0 -> ZERO | _ -> SUCC (__s3 (__s4 - 1))


let natmul (n1 : nat) (n2 : nat) : nat = __s3 (plus n1 * plus n2)
