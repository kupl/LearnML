type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check(inputlambda : lambda) : bool =
  let rec check((m : lambda),(validarea : var list)) : bool =
    match m with
    | V(var) -> List.mem var validarea
    | P(var, lambda) -> check(lambda, var :: validarea)
    | C(lambda1, lambda2) -> check(lambda1, validarea) && check(lambda2, validarea)
  in
  check(inputlambda, [])
