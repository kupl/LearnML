  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
  let check : lambda -> bool
  =fun e -> true

  let rec ch(e,l) =
  match e with
  |V(v) -> if List.mem v l then true else false
  |P(v,ex) -> ch(ex,l@[v])
  |C(e1,e2) -> ch(e1,l)&&ch(e2,l)

  let check e = ch(e,[])
