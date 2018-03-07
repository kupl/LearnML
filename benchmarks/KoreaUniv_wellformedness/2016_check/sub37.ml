exception NotImplemented

  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check : exp -> bool
  = fun exp ->
match exp with
| V v -> true
| P (v,p) -> true 
| _ -> raise NotImplemented (* TODO *)

