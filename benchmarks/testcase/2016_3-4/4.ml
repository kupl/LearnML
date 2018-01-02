type var = string

type exp =
  | V of var
  | P of var * exp
  | C of exp * exp

let rec check : exp -> bool
= fun exp ->
	match exp with
	| V _ -> false
	| C(_,_) -> false
	| P(x, V y) -> if x = y then true else false
	| P(x, P (y, z)) -> if x = y && check (P(x,z)) then true else false
	| P(x, C (y, z)) -> check (P(x,y)) && check (P(x,z))