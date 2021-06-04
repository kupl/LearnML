type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check : lambda -> bool =
 fun lambda ->
  match lambda with P (a, b) -> true | V a -> true | C (a, b) -> true
