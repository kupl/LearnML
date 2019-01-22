  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec compare v l= match l with
    [] -> false
    |hd::tl -> if hd=v then true else compare v tl

  let rec makel e l = match e with
     V(v)->compare v l
    |P(v, exp1)->makel exp1 (l@[v])
    |C(exp2, exp3)->makel exp2 l&&makel exp3 l

  let check : exp -> bool
   =fun e ->makel e []
