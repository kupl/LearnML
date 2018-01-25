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


let rec nl_trans exp tr_env =
		match exp with
		CONST n -> NL_CONST n |
		VAR a ->
			let temp_num = ref 0 in
			let rec cornum envc =
				match envc with
				[] -> raise (Failure "Freevariable") |
				h::t -> if h=a then NL_VAR (!temp_num)
											else ((temp_num := (!temp_num)+1); (cornum t)) in
				cornum tr_env |
		ADD (e1,e2) -> NL_ADD (nl_trans e1 tr_env, nl_trans e2 tr_env) |
		SUB (e1,e2) -> NL_SUB (nl_trans e1 tr_env, nl_trans e2 tr_env) |
		ISZERO e -> NL_ISZERO (nl_trans e tr_env) |
		IF (e1,e2,e3) -> NL_IF (nl_trans e1 tr_env,nl_trans e2 tr_env,nl_trans e3 tr_env) |
		LET (v,e1,e2) -> NL_LET (nl_trans e1 tr_env, nl_trans e2 (v::tr_env)) |
		PROC (v,e) -> NL_PROC (nl_trans e (v::tr_env)) |
		CALL (e1,e2) -> NL_CALL (nl_trans e1 tr_env, nl_trans e2 tr_env);;

let translate : program -> nl_program
=fun pgm ->
		nl_trans pgm [];;
