type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let pgm1 : lambda = P ("a", V "a")

let pgm2 : lambda = P ("a", V "b")

let rec check (lam : lambda) : bool =
  match lam with
  | P (v1, e1) -> (
      match e1 with
      | P (v2, e2) -> if v1 = v2 then true else false
      | V v2 -> if v1 = v2 then true else false
      | C (V v2, V v3) -> if v1 = v2 || v1 = v3 then true else false )


let (_ : bool) = check pgm1

let (_ : bool) = check pgm2
