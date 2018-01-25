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

  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list

  
let rec ncalc : nl_program -> nl_env -> nl_value
=fun pgm l ->
match pgm with
|NL_CONST i-> NL_Int i
|NL_VAR i-> nth l i
|NL_ADD (a,b)-> (match ncalc a l,ncalc b l with
		|NL_Int c, NL_Int d-> NL_Int(c + d)
		|_->raise (Failure "Type Error"))
|NL_SUB (a,b)-> (match ncalc a l, ncalc b l with
		|NL_Int c, NL_Int d-> NL_Int(c - d)
		|_->raise (Failure "Type Error"))
|NL_ISZERO a-> if ((ncalc a l)=NL_Int 0) then NL_Bool true else NL_Bool false
|NL_IF (a,b,c)-> if (ncalc a l)=NL_Bool true then (ncalc b l) else (ncalc c l)
|NL_LET (a,b)-> ncalc b ((ncalc a l)::l)
|NL_PROC a-> NL_Procedure(a,l)
|NL_CALL (e1,e2)->
	(match ncalc e1 l with
	|NL_Procedure(a,l0)-> ncalc a ((ncalc e2 l)::l0)
	|_->raise (Failure "Type Error")	
	)
	
  let nl_run : nl_program -> nl_value
  =fun pgm -> ncalc pgm []