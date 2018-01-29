
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let calculator : exp -> int
  = fun exp -> 
    let rec subFun : exp * exp -> int
    = fun (var, e) -> 
    match e with
    | X -> subFun (X, var)
    | INT i -> i
    | ADD (e1, e2) -> subFun (var, e1) + subFun (var, e2)
    | SUB (e1, e2) -> subFun (var, e1) - subFun (var, e2)
    | MUL (e1, e2) -> subFun (var, e1) * subFun (var, e2)
    | DIV (e1, e2) -> subFun (var, e1) / subFun (var, e2)
    | SIGMA (current, final, e) -> if current = final then subFun (current, e)
                                       else subFun (var, ADD (INT (subFun(current, e)), SIGMA (INT (subFun (X, current)+1), final, e)))
  in subFun (X, exp)