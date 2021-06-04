type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (a : lambda) : bool =
  match a with
  | V q -> false
  | P (q, p) -> (
      match p with
      | V x -> if q = x then true else false
      | P (x, y) -> (
          match y with
          | P (a, b) -> check (P (a, b)) || check (P (x, b)) || check (P (q, b))
          | V a -> a = x || a = q
          | C (a, b) -> (
              match (a, b) with
              | V a, V b -> (a = q || a = x) && (b = q || b = x)
              | V a, P (c, d) ->
                  (a = q || a = x)
                  && (check (P (c, d)) || check (P (x, d)) || check (P (q, d)))
              | P (a, b), V c ->
                  (check (P (a, b)) || check (P (x, b)) || check (P (q, b)))
                  && (c = q || c = x)
              | P (a, b), P (c, d) ->
                  (check (P (a, b)) || check (P (x, b)) || check (P (q, b)))
                  && (check (P (c, d)) || check (P (x, d)) || check (P (q, d)))
              | _, _ -> check a && check b ) )
      | C (x, y) -> (
          match (x, y) with
          | V a, V b -> a = q && b = q
          | V a, P (c, d) -> a = q && (check (P (c, d)) || check (P (q, d)))
          | P (a, b), V c -> (check (P (a, b)) || check (P (q, b))) && c = q
          | P (a, b), P (c, d) ->
              (check (P (a, b)) || check (P (q, b)))
              && (check (P (c, d)) || check (P (q, d)))
          | _, _ -> check x && check y ) )
  | C (q, p) -> check q && check p
