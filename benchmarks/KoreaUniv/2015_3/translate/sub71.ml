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

and env = (var) list

exception NOT_FOUND
let empty =[]
let rec lookup x e index = 
	match e with
	[]-> raise NOT_FOUND
	| (y)::t1->if x=y then index else lookup x t1 (index+1)
let extend x e = (x)::e


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




let rec translating = fun pgm env ->
		match pgm with
		|CONST int1-> NL_CONST int1
		|VAR var -> NL_VAR (lookup var env 0)
		|ADD (exp1,exp2) -> NL_ADD (translating exp1 env,translating exp2 env)
		|SUB (exp1,exp2) -> NL_SUB (translating exp1 env,translating exp2 env)
		|ISZERO exp -> NL_ISZERO (translating exp env)
		|IF (exp1,exp2,exp3)-> NL_IF(translating exp1 env, translating exp2 env, translating exp3 env)
		|CALL (exp1,exp2)-> NL_CALL(translating exp1 env, translating exp2 env)
		|PROC (var,exp1)-> NL_PROC(translating exp1 (extend var env) )
		|LET (var,exp1,exp2) -> NL_LET(translating exp1 env, translating exp2 (extend var env))	

let translate : program -> nl_program
=fun pgm -> translating pgm empty 
