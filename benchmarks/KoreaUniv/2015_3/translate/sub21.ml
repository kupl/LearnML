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
    
  let rec env : exp -> var list
  =fun e -> match e with
  			| LET (v1, c, e1) -> env e1 @ [v1]
  			| PROC (v1, e1) -> env e1
  			| _ -> []

  let rec cntScope : var * int * var list -> int
  =fun (v, n, lst) -> match lst with
  				      | [] -> raise (Failure "Empty")
  				      | head :: tail -> if head = v then n
  				      					else cntScope (v, n + 1, tail)  


  let rec trans : program * var list -> nl_program
  =fun (pgm1, env2) -> match pgm1 with
  			  | CONST c -> NL_CONST c
  			  | VAR v -> NL_VAR (cntScope (v, 0, env2))
  			  | ADD (e1, e2) -> NL_ADD (trans (e1,env2), trans (e2,env2))
  			  | SUB (e1, e2) -> NL_SUB (trans (e1, env2), trans (e2, env2))
  			  | ISZERO e -> NL_ISZERO (trans (e, env2))
  			  | IF (e1, e2, e3) -> NL_IF (trans (e1, env2), trans (e2, env2), trans (e3, env2))
  			  | LET (v, e1, e2) -> NL_LET (trans (e1, env2), trans (e2, v::env2))
  			  | PROC (v, e) -> NL_PROC (trans (e, v::env2))
  			  | CALL (e1, e2) -> NL_CALL (trans (e1, env2), trans (e2, env2))

  let translate : program -> nl_program
  =fun pgm -> trans (pgm, env pgm)
