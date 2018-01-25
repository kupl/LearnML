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

	type env = var list  

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
	
	let rec lex_dpth : var -> env -> int
	=fun var env -> match env with
		|	hd::tl -> if var = hd then 0 else 1 + lex_dpth var tl
		|	[] -> raise (Failure "Environment is empty")

	let rec transexp : exp -> env -> nl_exp
	=fun exp env -> match exp with
		| CONST int -> NL_CONST int
    | VAR x -> NL_VAR (lex_dpth x env)
    | ADD (e1, e2) -> NL_ADD (transexp e1 env, transexp e2 env)  
    | SUB (e1, e2) -> NL_SUB (transexp e1 env, transexp e2 env)
    | ISZERO e -> NL_ISZERO (transexp e env)
    | IF (e1, e2, e3) -> NL_IF (transexp e1 env, transexp e2 env, transexp e3 env)
    | LET (x, e1, e2) -> let env1 = x::env in NL_LET (transexp e1 env, transexp e2 env1)
    | PROC (x, e) -> let env1 = x::env in NL_PROC (transexp e env1)
    | CALL (e1, e2) -> NL_CALL (transexp e1 env, transexp e2 env)

  let translate : program -> nl_program
  =fun pgm -> transexp pgm []
