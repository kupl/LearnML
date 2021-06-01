  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
  let check : lambda -> bool
  =fun e -> true 

  let rec contain ex l=
  match ex with 
  |V(v)-> if List.mem v l then true else false
  |P(v,y)-> contain y (l@[v])
  |C(lambda1,lambda2) -> (contain lambda1 l)&&(contain lambda2 l)

let check ex =
  contain ex []
