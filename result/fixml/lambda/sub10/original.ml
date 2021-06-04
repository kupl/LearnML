type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check lambda =
  match lambda with
  | P (a, V b) -> a = b
  | P (a, C (b, c)) -> check (P (a, b)) && check (P (a, c))
  | P (a, P (b, c)) -> check (P (a, c))
  | C (_, _) -> false
  | V _ -> false
