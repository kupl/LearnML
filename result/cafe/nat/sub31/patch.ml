type nat = ZERO | SUCC of nat

let natadd (n1 : nat) (n2 : nat) : nat =
  let rec func_add (n1 : nat) (n2 : nat) : nat =
    match n1 with ZERO -> n2 | SUCC n1' -> SUCC (func_add n1' n2)
  in
  func_add n1 n2


let rec __s3 (__s4 : int) : nat =
  match __s4 with 0 -> ZERO | _ -> SUCC (__s3 (__s4 - 1))


let natmul (n1 : nat) (n2 : nat) : nat =
  let rec func_num (n : nat) : int =
    match n with ZERO -> 0 | SUCC n' -> 1 + func_num n'
  in

  let rec func_mul (a : nat) (b : int) : nat =
    match b with 0 -> a | 1 -> a | _ -> func_mul (natadd a n1) (b - 1)
  in
  __s3 (func_num n1 * func_num n2)
