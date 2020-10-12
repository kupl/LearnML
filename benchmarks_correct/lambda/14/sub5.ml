type lambda = 
  V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec check : lambda -> bool =
  fun lambda ->
    let rec checkSub : lambda * string list -> bool =
      fun (lambda, lst) ->
        match lambda with
        | V var -> List.mem var lst
        | P (var, lambda) -> checkSub (lambda, lst @ [var])
        | C (m1, m2) -> checkSub (m1, lst) && checkSub (m2, lst)
    in
    checkSub (lambda, [])

