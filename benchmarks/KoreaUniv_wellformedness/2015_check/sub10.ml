  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let check : exp -> bool
  =fun e -> true 

  let rec contain ex l=
  match ex with 
  |V(v)-> if List.mem v l then true else false
  |P(v,y)-> contain y (l@[v])
  |C(exp1,exp2) -> (contain exp1 l)&&(contain exp2 l)

let check ex =
  contain ex []
