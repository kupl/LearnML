type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec check (lambda, lst) =
  match lambda with P (var, m) -> check (m, var::lst)
  | V x -> List.mem x lst
  | C (a, b) -> check (a, lst) && check (b, lst)

let check lambda =
  check (lambda, [])

