type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check : lambda -> bool =
 fun lambda ->
  match lambda with V _ -> false | P (a, b) -> true | C (a, b) -> true
