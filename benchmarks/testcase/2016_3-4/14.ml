type var = string

type exp =
  | V of var
  | P of var * exp
  | C of exp * exp

let rec check : exp -> bool
= fun exp ->
match exp with
| V v -> true
| P (v,p) -> true 
| _ -> true