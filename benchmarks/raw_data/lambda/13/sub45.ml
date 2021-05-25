type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check lambda =
  let rec checkAux met area =
    match met with
    | V var -> List.mem var area
    | P (var, lambda) -> checkAux lambda (var::area)
    | C (m1, m2) -> (checkAux m1 area) && (checkAux m2 area) in
  checkAux lambda []
