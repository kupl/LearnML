type lambda = V of var
            |P of var * lambda
            |C of lambda * lambda
and var = string

let rec checkList (met, l) =
  match met with
  V n -> if (List.mem n l) then true else false
  |P (n, m) -> checkList(m, [n]@l)
  |C (m1, m2) -> checkList(m1, l) && checkList(m2, l)

let rec check met =
  checkList(met, [])


