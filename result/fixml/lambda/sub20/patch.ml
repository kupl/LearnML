type partial = FIRSTHALF | SECONDHALF

type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check lambda =
  match lambda with
  | V _ -> false
  | P (a, V s) -> if a = s then true else false
  | P (a, P (a2, C (m1, m2))) ->
      check (P (a2, C (m1, m2)))
      || (check (P (a, m1)) && true)
      || (check (P (a, m2)) && check (P (a2, m1)))
      || (check (P (a, m1)) && check (P (a, m2)))
  | P (a, P (a2, m)) -> check m || check (P (a2, m)) || check (P (a, m))
  | P (a, C (m1, m2)) -> check (P (a, m1)) && check (P (a, m2))
  | C (V _, _) -> false
  | C (_, V _) -> false
  | C (P (a1, m1), P (a2, m2)) -> check (P (a1, m1)) && check (P (a2, m2))
  | C (C (m1, m2), C (m3, m4)) -> check m1 && check m2 && check m3 && check m4
  | C (C (m1, m2), P (a, m)) -> check m1 && check m2 && check (P (a, m))
  | C (P (a, m), C (m1, m2)) -> check (P (a, m)) && check m1 && check m2
