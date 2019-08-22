
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec check : lambda -> bool
  = fun lambda ->
			match lambda with
| V _ -> false
| C(_,_) -> false
| P(x, V y) -> if x = y then true else false
| P(x, P (y, z)) -> if x = y && check (P(x,z)) then true else false
| P(x, C (y, z)) -> check (P(x,y)) && check (P(x,z))
