  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec check : exp -> bool
  =fun e -> match e with
| V x -> true
| P(x,y) -> check(y) && checkx(x,y)
| C(x,y) -> check(y) && check(x) 

and checkx (a,b) = match b with
| V x -> if x=a then true else false
| P(x,y) -> checkx(a,y)
| C(x,y) -> checkx(a,x) || checkx(a,y)
