exception Error

type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let check met =
let rec check_in met lst = match met with V(var) -> (List.mem var lst)
| P (var, met2) -> check_in met2 (lst@[var])
| C (met1, met2) -> (check_in met1 lst) && (check_in met2 lst) in
check_in met [];;