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
 
let rec nth : nl_env*int -> nl_value
= fun (l,n) -> match l with 
|[] -> raise(Failure "error")
|hd::tl -> if(n=0) then hd else nth(tl,n-1)

let rec eval : nl_program*nl_env -> nl_value 
=fun (pro,env) -> match pro with
|NL_CONST n -> NL_Int n
|NL_VAR x -> nth(env,0)
|NL_ADD (e1,e2) -> let a1 =eval(e1,env)
								in let a2=eval(e2,env)
								in (match a1,a2 with
								|NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
								|_->raise(Failure"error"))
|NL_SUB(e1,e2)->let a1=eval(e1,env)
							in let a2=eval(e2,env)
							in (match a1,a2 with
							|NL_Int n1,NL_Int n2 -> NL_Int (n1-n2)
							|_->raise(Failure("error")))
|NL_ISZERO e ->let a=eval(e,env) in (match a with
							|NL_Int n -> if n=0 then NL_Bool true else NL_Bool false
							|_->raise(Failure("error")))
|NL_IF(e1,e2,e3)->let a=eval(e1,env) in (match a with
							|NL_Bool true -> eval(e2,env)
							|_-> eval(e3,env))
|NL_LET(e1,e2) -> let v1 =eval(e1,env) in
									eval(e2,v1::env)
|NL_PROC(e) -> NL_Procedure(e,env)
|NL_CALL(e1,e2) -> let p = eval(e1,env) in 
								let v=eval(e2,env) in (match p with
								|NL_Procedure( e,ev1) -> eval(e,v::ev1)
								|_-> raise(Failure "error"))						
	
  let nl_run : nl_program -> nl_value
  =fun pgm -> eval(pgm,[])