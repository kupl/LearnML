
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec set_var : exp -> var list -> bool
  = fun exp str -> match exp with 
  | V r -> if List.mem r str then true else false
  | P (aexp, e) -> set_var e ([aexp;]@str)
  | C (e1, e2) -> set_var e1 str && set_var e2 str

  let rec check : exp -> bool
  = fun exp -> match exp with
  | V str -> false
  | P (str, e) -> set_var e [str;]
  | C (e1, e2) -> check e1 && check e2 (* TODO *)
