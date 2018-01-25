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

let rec getValue : int -> nl_env -> nl_value
= fun i env ->
	match env with
	| [] -> raise (Failure "Empty environment")
	| hd::tl -> if i = 0 then hd else getValue (i-1) tl


let add_NL_Int
= fun ni1 ni2 ->
		match ni1 with 
		|NL_Int (a) ->	
			(match ni2 with 
			 |NL_Int (b) -> NL_Int(a + b)
			 |_ -> raise (Failure "error"))
		|_ -> raise (Failure "error")

let sub_NL_Int
= fun ni1 ni2 ->
		match ni1 with 
		|NL_Int (a) ->	
			(match ni2 with 
			 |NL_Int (b) -> NL_Int(a - b)
			 |_ -> raise (Failure "error"))
		|_ -> raise (Failure "error")


let rec _nl_run : nl_program -> nl_env -> nl_value
= fun pgm env ->
	match pgm with
	| NL_CONST (i) -> NL_Int (i)
	| NL_VAR (i) -> (getValue i env)
	| NL_ADD (e1, e2) -> add_NL_Int (_nl_run e1 env) (_nl_run e2 env)
	| NL_SUB (e1, e2) -> sub_NL_Int (_nl_run e1 env) (_nl_run e2 env)
	| NL_ISZERO (e) -> if (_nl_run e env) = NL_Int (0) then NL_Bool (true)
					   else NL_Bool (false)
	| NL_IF (e1, e2, e3) -> if (_nl_run e1 env) = NL_Bool (true) 
								then _nl_run e2 env 
								else _nl_run e3 env
	| NL_LET (e1, e2) -> _nl_run e2 ((_nl_run e1 env)::env)
	| NL_PROC (e) -> NL_Procedure (e, env)
	| NL_CALL (e1, e2) ->
			let NL_Procedure (procExp, procEnv) = _nl_run e1 env
			in _nl_run procExp ((_nl_run e2 env)::procEnv)

let nl_run : nl_program -> nl_value
=fun pgm -> 
	_nl_run pgm []

(* 
open Problem4;;
open Problem5;;
let myPgm = (LET ("myFun", 
				   PROC("x", ADD( VAR "x", CONST 10)),
				   LET("x",
 				       CONST 7, 
					   CALL (VAR "myFun", VAR"x"))))
let myPgm2 = (CALL 
				(CALL 
					(PROC("x", 
						  PROC("y", 
							   SUB(VAR "x", VAR "y"))),
  				     CONST 5),
				 CONST 10))

let myNlPgm = translate myPgm ;;
let myNlPgm2 = translate myPgm2 ;;

nl_run myNlPgm ;;
nl_run myNlPgm2 ;; *)
 