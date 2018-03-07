
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check : exp -> bool
  = fun exp ->
		match exp with
		| V v -> true
		| P (v, e) -> true
		| C (e1, e2) -> true
