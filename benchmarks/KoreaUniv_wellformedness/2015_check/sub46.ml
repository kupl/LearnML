  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string;;

  module SS = Set.Make(String);;

  let rec echeck : exp*SS.t -> bool
  =fun (e, vars) -> match e with
    | V variable -> not (SS.is_empty (SS.inter (SS.singleton variable) vars))
    | P (variable,expression) -> echeck (expression,SS.add variable vars)
    | C (expression1,expression2) -> echeck (expression1,vars) && echeck(expression2,vars);;

  let check : exp -> bool
  =fun e -> echeck (e,SS.empty);;
