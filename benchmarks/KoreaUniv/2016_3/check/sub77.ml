
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string
    
let rec check : exp -> bool
   = fun exp -> 
   match exp with
   |P(a, b) ->true
   |V a -> true
   |C(a,b) -> true
