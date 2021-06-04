type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (lambda : lambda) : bool =
  match lambda with
  | V s -> false
  | P (n, m) -> (
      match m with
      | V s -> if n = s then true else false
      | P (n, m) -> check (P (n, m))
      | C (m1, m2) -> check (P (n, m1)) || check (P (n, m2)) )
  | C (m1, m2) -> false
