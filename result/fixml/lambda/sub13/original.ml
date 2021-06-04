type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check a =
  match a with
  | V _ -> raise Invalid_argument "check"
  | C (a, b) -> raise Invalid_argument "check"
  | P (a, V b) -> if a = b then true else false
  | P (a, C (V b, V c)) -> if a = b || a = c then true else false
  | P (a, C (P (b, V c), V d)) -> if a = c || a = d then true else false
  | P (a, C (V b, P (c, V d))) -> if a = b || a = d then true else false
  | P (a, b) -> check b
