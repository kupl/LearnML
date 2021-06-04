type nat = ZERO | SUCC of nat

let natadd (n1 : nat) (n2 : nat) : nat =
  let rec func_add (n1 : nat) (n2 : nat) : nat =
    match n1 with ZERO -> n2 | SUCC n1' -> SUCC (func_add n1' n2)
  in
  func_add n1 n2


let natmul (n1 : nat) (n2 : nat) : nat =
  let rec func_num (n : nat) : int =
    match n with ZERO -> 0 | SUCC n' -> 1 + func_num n'
  in

  let rec func_mul (a : nat) (b : int) : nat =
    match b with 0 -> a | 1 -> a | _ -> func_mul (natadd a n1) (b - 1)
  in
  func_mul n1 (func_num n2)
