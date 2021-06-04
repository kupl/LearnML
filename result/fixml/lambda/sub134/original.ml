type lambda = V of var | P of var * lambda | C of lambda * lambda

and var = string

let pgm1 = P ("a", V "a")

let pgm2 = P ("a", V "b")

let rec check : lambda -> bool =
 fun lam ->
  match lam with
  | P (v1, e1) -> (
      match e1 with
      | P (v2, e2) -> if v1 = v2 then true else false
      | V v2 -> if v1 = v2 then true else false
      | C (V v2, V v3) -> if v1 = v2 || v1 = v3 then true else false )


;;
check pgm1

;;
check pgm2
