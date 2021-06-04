type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (x : lambda) : bool =
  match x with
  | P (n, V n1) -> if n1 = n then true else false
  | P (n, P (n1, m)) -> check (P (n, m)) && check (P (n1, m))
  | P (n, C (V m1, V m2)) -> if n = m1 || n = m2 then true else false
  | P (n, C (P (n1, m1), V n2)) ->
      if (n = n2 || n1 = n2) && (check (P (n, m1)) || check (P (n1, m1))) then
        true
      else false
  | P (n, C (V n2, P (n1, m1))) ->
      if (n = n2 || n1 = n2) && (check (P (n, m1)) || check (P (n1, m1))) then
        true
      else false
  | C (P (n1, m1), P (n2, m2)) ->
      if
        (check (P (n1, m1)) || check (P (n2, m1)))
        && (check (P (n1, m2)) || check (P (n2, m2)))
      then true
      else false
  | V n -> false
