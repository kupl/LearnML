  type program = exp
  and exp = 
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string
  
  type nl_program = nl_exp
  and nl_exp = 
    | NL_CONST of int
    | NL_VAR of int
    | NL_ADD of nl_exp * nl_exp
    | NL_SUB of nl_exp * nl_exp
    | NL_ISZERO of nl_exp
    | NL_IF of nl_exp * nl_exp * nl_exp
    | NL_LET of nl_exp * nl_exp
    | NL_PROC of nl_exp 
    | NL_CALL of nl_exp * nl_exp
  
  let rec findx : var-> var list -> int -> int
  = fun x env cnt -> match env with
  | [] -> raise (Failure"error")
  | hd::tl -> if x = hd then cnt
  else findx x tl (cnt+1)


  let rec trans : exp -> var list -> nl_exp
  = fun exp env -> match exp with
  | CONST n -> NL_CONST n
  | VAR x -> NL_VAR (findx x env 0)
  | ADD (e1,e2) -> NL_ADD ((trans e1 env),(trans e2 env))
  | SUB (e1,e2) -> NL_SUB ((trans e1 env),(trans e2 env))
  |ISZERO e -> NL_ISZERO (trans e env)
  |IF (e1,e2,e3) -> NL_IF (trans e1 env ,trans e2 env ,trans e3 env)
  |LET (x, e1,e2) -> NL_LET (trans e1 env , trans e2 (x::env))
  |PROC(x,e1) -> NL_PROC (trans e1 (x::env))
  |CALL(e1,e2) -> NL_CALL(trans e1 env ,trans e2 env)

  let translate : program -> nl_program
  =fun pgm -> trans pgm []
