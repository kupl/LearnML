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


 
  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list


let rec bfr : nl_exp -> nl_env -> nl_value=fun prg env
-> match prg with  
  
  | NL_CONST i->  NL_Int i 
 
  | NL_VAR v ->  List.nth env v
 
  | NL_ADD (e1,e2) -> let v1=(bfr e1 env ) in let v2=(bfr e2 env) in (match v1,v2 with		
			|NL_Int n1, NL_Int n2->NL_Int (n1+n2)
			|_-> raise(Failure"error"))
 | NL_SUB (e1,e2) -> let v1=(bfr e1 env ) in let v2=(bfr e2 env) in (match v1,v2 with		
			|NL_Int n1, NL_Int n2->NL_Int (n1-n2)
			|_-> raise(Failure"error"))
			
  | NL_ISZERO e-> (match e with 
			|NL_CONST 0-> NL_Bool true
			|_ -> NL_Bool false
				)
  
  | NL_IF (e1,e2,e3) ->(match (bfr e1 env) with 
			| NL_Bool true -> (bfr e2 env)
			| NL_Bool false -> (bfr e3 env)
			|_-> raise (Failure "error")
						)
			
  |NL_LET (e1,e2)-> let a = bfr e1 env in (bfr e2 (extend_env a env))
  
  | NL_PROC (e1) -> NL_Procedure (e1, env)

	

  | NL_CALL (e1,e2) -> (match bfr e1 env with 
						| NL_Procedure (e3,env2)->(match bfr e2 env with 
									|v2-> bfr e3 (extend_env v2 env2))
									|_->raise (Failure "error") )
              
 
let nl_run : nl_program -> nl_value
   =fun pgm -> bfr pgm []  