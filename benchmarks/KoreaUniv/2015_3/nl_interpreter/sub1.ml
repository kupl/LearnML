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

let rec getElement : int->nl_env->nl_value=
fun num env->
match env with
|[]->raise(Failure "overFlow")
|hd::tl->if num =0 then hd else getElement (num-1) tl

let rec nl_eval : nl_program->nl_env->nl_value=
fun pgm env-> match pgm with
| NL_CONST a ->NL_Int a
| NL_VAR a -> getElement a env
|NL_ADD (e1, e2)->
let v1 = nl_eval e1 env in
let v2 = nl_eval e2 env in
(match v1, v2 with
 |NL_Int n1, NL_Int n2 -> NL_Int(n1+n2)
 |_->raise (Failure "Type Error : non-numeric values"))
|NL_SUB (e1, e2)->
let v1 = nl_eval e1 env in 
let v2 = nl_eval e2 env in
(match v1, v2 with
 |NL_Int n1, NL_Int n2 -> NL_Int(n1-n2)
 |_->raise (Failure "Type Error : non-numeric values"))
|NL_ISZERO e ->
(match nl_eval e env with
 |NL_Int n when n =0 ->NL_Bool true
 |_->NL_Bool false)
|NL_IF (e1, e2, e3)->
(match nl_eval e1 env with
 |NL_Bool true -> nl_eval e2 env
 |NL_Bool false -> nl_eval e3 env
 |_ -> raise (Failure "Type Error : condition must be Bool type"))
|NL_LET(e1, e2)->
let v1 = nl_eval e1 env in 
nl_eval e2 (v1::env)

|NL_PROC (e1)->NL_Procedure (e1, env) 

|NL_CALL (e1, e2)-> 
(match nl_eval e1 env with 
 |NL_Procedure (e, env1)-> 
 let v= nl_eval e2 env in
   nl_eval e (v::env1)
 |_->raise(Failure "Type Error") )

let rec nl_run : nl_program -> nl_value
=fun pgm -> nl_eval pgm []
