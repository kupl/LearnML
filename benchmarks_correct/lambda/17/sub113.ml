type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec check: lambda -> bool = fun (input) ->
  let rec check2: lambda * var list -> bool = fun (input, vars) ->
    match input with
    | V n -> List.mem n vars
    | P (n, m) -> check2(m, n :: vars)
    | C (m1, m2) -> check2(m1, vars) && check2(m2, vars)
  in
  check2(input, [])