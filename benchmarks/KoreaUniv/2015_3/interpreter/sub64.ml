type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | ISZERO of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

type value = Int of int | Bool of bool 
           | Procedure of var * exp * env 
           | RecProcedure of var * var * exp * env
and env = var -> value

let empty_env = fun _ -> raise (Failure "Environment is empty")
let extend_env (x,v) e = fun y -> if x = y then v else (e y)
let apply_env e x = e x

let rec eval exp env =
		match exp with
		CONST n -> Int n |
		VAR x -> apply_env env x |
		ADD (e1,e2) -> 
			let v1 = eval e1 env in let v2 = eval e2 env in
			(match (v1,v2) with
			(Int n1, Int n2) -> Int (n1+n2) |
			(_,_) -> raise (Failure "Type-Error")) |
		SUB (e1,e2) ->
			let v1 = eval e1 env in let v2 = eval e2 env in
			(match (v1,v2) with
			(Int n1, Int n2) -> Int (n1-n2) |
			(_,_) -> raise (Failure "Type-Error")) |
		ISZERO e ->
			(match (eval e env) with
			Int n when n=0 -> Bool true |
			_ -> Bool false) |
		IF (e1,e2,e3) ->
			(match (eval e1 env) with
			Bool true -> eval e2 env |
			Bool false -> eval e3 env |
			_ -> raise (Failure "Type-Error")) |
		LET (x,e1,e2) ->
			let v1 = eval e1 env in
				eval e2 (extend_env (x,v1) env) |
		LETREC (st,x,e1,e2) ->
				eval e2 (extend_env (st, RecProcedure (st,x,e1,env)) env) |
		PROC (x,e) -> Procedure (x,e,env) |
		CALL (e1,e2) ->
			(match eval e1 env with
			Procedure (xp,ep,envp) -> eval ep (extend_env (xp, (eval e2 env)) envp) |
			RecProcedure (stp,xp,ep,envp) ->
					let env1 = extend_env (xp, (eval e2 env)) envp in
					let env2 = extend_env (stp, RecProcedure(stp,xp,ep,envp)) env1 in
					eval ep env2 |
			_ -> raise (Failure "Type-Error"));;

let run : program -> value
=fun pgm -> eval pgm empty_env;;
