type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec check : lambda -> bool = function x -> (
  let rec check_temp x (l : var list) : bool = match x with
  | V n -> List.exists (fun x -> x = n) l
  | P (n, m) -> check_temp m (n::l)
  | C (m1, m2) -> if (check_temp m1 l) then (check_temp m2 l) else false
  in check_temp x []
)
