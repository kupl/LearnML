type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec chars (e : lambda) : var =
  match e with
  | V a -> a
  | P (v, e1) -> (
      match e1 with V b -> b | P (b, e2) -> chars e2 | C (e1, e2) -> chars e2 )
  | C (e1, e2) -> (
      match (e1, e2) with V b, V c -> c | V b, P (c, e3) -> chars e3 )


let rec check (e : lambda) : bool =
  match e with
  | V a -> true
  | P (v, e1) -> (
      match e1 with
      | V b -> if v = b then true else false
      | P (b, e2) -> if chars e2 = v || chars e2 = b then true else false
      | C (e1, e2) -> if chars e1 = v || chars e2 = v then true else false )
  | C (e1, e2) -> (
      match (e1, e2) with
      | V b, V c -> true
      | V b, P (c, e3) -> if c = b || b = chars e3 then true else false )
