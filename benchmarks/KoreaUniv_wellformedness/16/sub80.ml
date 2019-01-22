
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string
  
  let rec exist v lst = match lst with 
  | [] -> false
  | hd::tl ->  if (v = hd) then true else ( exist v tl) 
 
  let rec checker e lst = match e with
  | V (v) -> exist v lst 
  | P (v,e) -> checker e (lst@[v])
  | C(e1,e2) ->if ((checker e1 lst) = (checker e2 lst)) then true else false

  let rec check : exp -> bool
  = fun exp -> checker exp []
