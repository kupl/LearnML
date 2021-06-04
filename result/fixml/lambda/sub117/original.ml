type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check : lambda -> bool =
 fun lambda ->
  match lambda with
  | V a -> false
  | P (a, b) -> (
      match lambda with
      | V c -> if a = c then true else false
      | P (c, d) -> check (P (c, d)) || check (P (a, d))
      | C (c, d) -> check (P (a, c)) && check (P (a, d)) )
  | C (a, b) -> (
      match (a, b) with
      | P (c, d), P (e, f) -> check (P (c, d)) && check (P (e, f))
      | _ -> false )
