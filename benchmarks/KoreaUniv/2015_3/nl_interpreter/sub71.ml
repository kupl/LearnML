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

exception NOT_FOUND
let nl_empty = []
let rec nl_lookup index e = match e with
				[]->raise NOT_FOUND
				|hd::t1-> if index=0 then hd else nl_lookup (index-1) e
let nl_extend v e = v::e

let rec interprete_bop = fun op exp1 exp2 env ->
				let v1 = interprete exp1 env in
				let v2 = interprete exp2 env in
				(match v1,v2 with
				 |NL_Int int1,NL_Int int2 -> NL_Int (op int1 int2)

				 |_->raise (Failure "NOT_MATCH")
				 )


and interprete = fun exp env -> match exp with					
					|NL_CONST int1-> NL_Int int1
					|NL_VAR int1-> nl_lookup int1 env
					|NL_ADD (exp1,exp2)-> interprete_bop (+) exp1 exp2 env
					|NL_SUB (exp1,exp2)-> interprete_bop (-) exp1 exp2 env
					|NL_ISZERO exp ->( let v1 = interprete exp env in
							match v1 with
							|NL_Int n->if n=0 then NL_Bool true else NL_Bool false
							|_->raise(Failure "Type Error")
							)
					|NL_IF (exp1,exp2,exp3)->(match interprete exp1 env with
								|NL_Bool true ->interprete exp2 env
								|NL_Bool false->interprete exp3 env
								|_->raise(Failure "Type_Error")
							)
					|NL_LET (exp1,exp2)-> interprete exp2 (nl_extend (interprete exp1 env) env)
					|NL_PROC exp -> NL_Procedure(exp,env)
									
					|NL_CALL (exp1,exp2) ->(match interprete exp1 env with
											|NL_Procedure (pexp,envprime)->
												let v = interprete exp2 env in
													interprete pexp (nl_extend (v) envprime )
								|_->raise (Failure "Type_Error")	
							)

let nl_run : nl_program -> nl_value
=fun pgm -> interprete pgm nl_empty
