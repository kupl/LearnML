type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check lambda =
  match lambda with
  | V var -> false
  | P (var1, V var2) -> var1 = var2
  | P (var1, P (var2, C (lambda1, lambda2))) ->
      (check (P (var1, lambda1)) && true)
      || (check (P (var1, lambda2)) && check (P (var2, lambda1)))
  | P (var1, P (var2, lambda)) ->
      check (P (var1, lambda)) || check (P (var2, lambda))
  | P (var, C (lambda1, lambda2)) ->
      check (P (var, lambda1)) && check (P (var, lambda2))
  | C (lambda1, lambda2) -> check lambda1 && check lambda2
