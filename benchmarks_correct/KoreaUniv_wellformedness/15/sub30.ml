  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  
  let rec lst_del x l =
    match l with
    | [] -> l
    | h::t -> if (h = x) then lst_del x t else h::(lst_del x t)
  let rec _check exp =
    match exp with
    | V x -> [x]
    | P (x, e) -> lst_del x (_check e)
    | C (e1, e2) -> (_check e1)@(_check e2)
  
  let check : exp -> bool
  =fun e -> if (_check e = []) then true else false
