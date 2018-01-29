
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec chklst (exp,lst) =
  match exp with (* should be call by value *)
  | P(var,exp) -> chklst(exp, lst@[var])
  | C(exp1,exp2) -> if(chklst(exp1, lst) && chklst(exp2, lst)) then true else false
  | V var -> if lst=[] then false
    else List.exists (fun x -> (x=var)) lst

  let check : exp -> bool
  = fun exp -> chklst (exp,[] )
