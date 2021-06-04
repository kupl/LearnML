type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (lambda : lambda) : bool =
  match lambda with V v -> true | P (v, e) -> true | C (e1, e2) -> true
