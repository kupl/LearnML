type lambda = V of var | P of var * lambda | C of lambda * lambda

and var = string

let rec check : lambda -> bool =
 fun lambda ->
  match lambda with
  | V vari -> true
  | P (vari, lambda) -> (
      match lambda with
      | V v -> if vari = v then true else false
      | C (e1, e2) ->
          (check (P (vari, e1)) || check (P (vari, e2))) && check e1 && check e2
      | P (v, e) -> check (P (v, e)) && check (P (vari, e)) )
  | C (lambda1, lambda2) -> check lambda1 && check lambda2
