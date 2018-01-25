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

let rec find_var: int * nl_env -> nl_value
=fun (a,lst) ->
match lst with
[]-> raise (Failure "syntax error")
|h::t -> if(a=0) then h else find_var(a-1,t)

let rec nl_eval : nl_program * nl_env -> nl_value
=fun (pgm,env) ->
match pgm with
NL_CONST a-> NL_Int a
|NL_VAR a-> find_var(a,env)
|NL_ADD (ex1,ex2) ->
let v1= nl_eval (ex1,env) in
let v2= nl_eval (ex2,env) in
(
  match v1,v2 with
  | NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
  |_ -> raise (Failure "Environment is empty")
) 
|NL_SUB (ex1,ex2) ->
let v1= nl_eval (ex1,env) in
let v2= nl_eval (ex2,env) in
(
  match v1,v2 with
  | NL_Int n1, NL_Int n2 -> NL_Int (n1-n2)
  |_ -> raise (Failure "Environment is empty")
) 
|NL_ISZERO (ex) ->
(
  match nl_eval(ex,env) with
  NL_Int n -> if(n=0) then NL_Bool true else NL_Bool false
  |_-> raise (Failure "syntax error")
)
|NL_IF (ex1,ex2,ex3) ->
(
  match nl_eval(ex1,env) with
  NL_Bool b-> if(b=true) then nl_eval(ex2,env) else nl_eval(ex3,env)
  |_-> raise (Failure "syntax error")
)
|NL_LET (ex1,ex2) ->
(
  let v=nl_eval(ex1,env) in
  nl_eval(ex2,v::env)
)
|NL_PROC (ex) -> NL_Procedure(ex,env)
|NL_CALL (ex1,ex2) ->
match nl_eval(ex1,env) with
NL_Procedure(e,env2)-> let v=nl_eval(ex2,env) in nl_eval(e,v::env2)
|_ -> raise (Failure "Can't find") 

let nl_run : nl_program -> nl_value
=fun pgm -> (* TODO *)
nl_eval(pgm,[])
