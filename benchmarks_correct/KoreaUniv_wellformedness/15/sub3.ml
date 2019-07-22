  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let check : exp -> bool
  =fun e -> true

  let rec ch(e,l) =
  match e with
  |V(v) -> if List.mem v l then true else false
  |P(v,ex) -> ch(ex,l@[v])
  |C(e1,e2) -> ch(e1,l)&&ch(e2,l)

  let check e = ch(e,[])
