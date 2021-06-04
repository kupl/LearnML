type var = string

type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

let rec apl (b, m) =
  match (b, m) with
  | b, V a -> if b = a then P (b, V a) else V a
  | b, P (a, V c) -> if a != c && b = c then P (b, V c) else P (a, V c)
  | b, P (a, P (c, d)) -> P (a, P (c, apl (b, C (d, d))))
  | b, P (a, C (c, d)) -> P (a, apl (b, C (c, d)))
  | b, C (a, c) -> C (apl (b, a), apl (b, c))


let rec check m =
  match m with
  | P (a, V b) -> if a = b then true else false
  | P (a, C (b, c)) -> check (P (a, b)) && check (P (a, c))
  | P (a, P (b, V c)) -> check (P (a, V c)) || check (P (b, V c))
  | P (a, P (b, c)) -> check (apl (b, P (a, c))) || check (apl (a, P (b, c)))
  | C (a, b) -> check a && check b
  | _ -> false
