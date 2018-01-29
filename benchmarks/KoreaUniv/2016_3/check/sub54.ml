(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec varOfExp : exp -> var = fun exp ->
   match exp with
   |V var -> var
   |P(var,e) -> varOfExp(e)
   |C(e1,e2) -> varOfExp(e2)


   let check : exp -> bool
  = fun exp -> match exp with
  |P(var1,P(var2,e)) -> if (varOfExp e = var2) || (varOfExp e = var1) then true else false
  |P(var1,C(e1,e2)) -> if (varOfExp e2 = var1) then true else false
  |P(var1,V var2) -> if var1= var2 then true else false
  |_ -> raise NotImplemented
