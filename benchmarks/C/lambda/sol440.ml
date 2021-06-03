  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
  let rec ch_list(lambda, lst) =
  match lambda with
  |V(v) -> if List.mem v lst then true else false
  |P(v,e) -> ch_list(e, lst@[v])
  |C(e1, e2) -> ch_list(e1,lst) && ch_list(e2, lst)

let check : lambda -> bool
=fun e -> ch_list(e, [])
