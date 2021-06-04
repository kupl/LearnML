type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let check (lambda : lambda) : bool =
  let rec e (lambda : lambda) : bool =
    match lambda with
    | V v -> false
    | P (p, l) ->
        let rec f (p : string) (l : lambda) : bool =
          match l with
          | V a -> if a = p then true else false
          | P (b, c) -> if f p c || f b c then true else false
          | C (d, e) -> if f p e && f p d then true else false
        in
        f p l
    | C (n, m) -> (
        match (n, m) with
        | V x, V y -> false
        | P (x, y), P (z, t) ->
            if (e (P (x, y)) || e (P (z, y))) && (e (P (x, t)) || e (P (z, t)))
            then true
            else false
        | V x, P (y, z) ->
            if (e (P (y, n)) || e (C (n, z))) && (e (P (y, z)) || e (C (n, z)))
            then true
            else false
        | P (y, z), V x ->
            if (e (P (y, m)) || e (C (m, z))) && (e (P (y, z)) || e (C (m, z)))
            then true
            else false
        | V x, C (y, z) ->
            if
              (e (C (n, y)) || e (C (n, z)))
              && (e (C (y, z)) || e (C (y, n)))
              && (e (C (z, n)) || e (C (z, y)))
            then true
            else false
        | C (y, z), V x ->
            if
              (e (C (m, y)) || e (C (m, z)))
              && (e (C (y, z)) || e (C (y, m)))
              && (e (C (z, m)) || e (C (z, y)))
            then true
            else false
        | P (x, y), C (z, t) ->
            if
              (e (P (x, y)) || e (C (y, z)) || e (C (y, t)))
              && (e (P (x, z)) || e (C (z, t)) || e (C (z, y)))
              && (e (P (x, t)) || e (C (t, y)) || e (C (t, z)))
            then true
            else false
        | C (z, t), P (x, y) ->
            if
              (e (P (x, y)) || e (C (y, z)) || e (C (y, t)))
              && (e (P (x, z)) || e (C (z, t)) || e (C (z, y)))
              && (e (P (x, t)) || e (C (t, y)) || e (C (t, z)))
            then true
            else false
        | C (x, y), C (z, t) ->
            if
              (e (C (x, y)) || e (C (x, z)) || e (C (x, t)))
              && (e (C (y, z)) || e (C (y, t)) || e (C (y, x)))
              && (e (C (z, t)) || e (C (z, x)) || e (C (z, y)))
              && (e (C (t, x)) || e (C (t, y)) || e (C (t, z)))
            then true
            else false )
  in
  e lambda
