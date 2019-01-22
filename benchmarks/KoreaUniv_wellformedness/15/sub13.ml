  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec ch_list(exp, lst) =
  match exp with
  |V(v) -> if List.mem v lst then true else false
  |P(v,e) -> ch_list(e, lst@[v])
  |C(e1, e2) -> ch_list(e1,lst) && ch_list(e2, lst)

let check : exp -> bool
=fun e -> ch_list(e, [])
