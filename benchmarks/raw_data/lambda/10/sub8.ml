type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec check2 (lambda, lst) =
  match lambda with P (var, m) -> check2 (m, var::lst)
  | V x -> List.mem x lst
  | C (a, b) -> check2 (a, lst) && check2 (b, lst)

let check lambda =
  check2 (lambda, [])

