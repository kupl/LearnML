type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (m : lambda) =
  match m with
  | P (a, V b) -> if a = b then true else false
  | P (a, P (b, c)) ->
      if a = b then check (P (b, c))
      else if check (P (a, c)) = true then true
      else if check (P (b, c)) = true then true
      else false
  | P (a, C (b, c)) ->
      if check (P (a, b)) = true then
        if check (P (a, c)) = true then true else false
      else false
