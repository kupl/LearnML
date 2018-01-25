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

  exception NoSuchArgument

  let rec search_index : var list -> var -> int -> int
  =fun l v i ->
	match l with
		| [] -> raise NoSuchArgument 
		| hd::tl -> if hd = v then i else search_index tl v (i+1)

  let index_of : var list -> var -> int
  =fun l v -> search_index l v 0 

  let rec converter : program -> var list -> nl_program
  =fun e env ->
	match e with 
		| CONST i -> NL_CONST i
		| VAR v -> NL_VAR (index_of env v) 
		| ADD (e1,e2) -> NL_ADD ((converter e1 env), (converter e2 env))
		| SUB (e1,e2) -> NL_SUB ((converter e1 env), (converter e2 env))
		| ISZERO e -> NL_ISZERO (converter e env) 
		| IF (e1,e2,e3) -> NL_IF ((converter e1 env), (converter e2 env), (converter e3 env)) 
		| LET (x,e1,e2) -> NL_LET ((converter e1 env), (converter e2 (x::env)))
		| PROC (x,e) -> NL_PROC (converter e (x::env))
		| CALL (e1,e2) -> NL_CALL ((converter e env), (converter e env)) 

  let translate : program -> nl_program
  =fun pgm -> converter pgm []
