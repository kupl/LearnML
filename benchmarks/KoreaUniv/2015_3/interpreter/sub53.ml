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
  
  let empty_env = fun _ -> raise (Failure "empty environment")
  let extend_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_env e x = e x

  let rec eval : exp -> env -> value
	=fun exp env -> 
	match exp with
	|CONST n -> Int n
 	|VAR x -> apply_env env x
	|ADD (exp1, exp2) ->
		let v1 = eval exp1 env in
		let v2 = eval exp2 env in
			(match v1, v2 with
			|Int n1, Int n2 -> Int(n1 + n2)
			|_ -> raise(Failure "Type Error : non-numeric values"))
	|SUB(exp1, exp2) ->
		let v1 = eval exp1 env in
		let v2 = eval exp2 env in
			(match v1, v2 with
			|Int n1, Int n2 -> Int(n1 - n2)
			|_ -> raise(Failure "Type Error : non-numeric values"))
	|ISZERO exp -> 
		(match eval exp env with
		|Int n when n=0 -> Bool true
		|_ -> Bool false)
	|IF (exp1, exp2, exp3) ->
		(match eval exp1 env with
		|Bool true -> eval exp2 env
		|Bool false -> eval exp3 env
		|_ -> raise(Failure "Type Error : condition must be Bool Type"))
	|LET (x, exp1, exp2) ->
		let v1 = eval exp1 env in 
		eval exp2 (extend_env (x, v1) env) 
	|LETREC (f, x, exp1, exp2) -> eval exp2 (extend_env (f, RecProcedure(f, x, exp1, env)) env)
	|PROC (x, exp) -> Procedure (x, exp, env)
	|CALL (exp1, exp2) -> 
		(match eval exp1 env with
		|Procedure(x, exp, env1) ->
			let v = eval exp2 env in
			eval exp (extend_env(x, v) env1)
		|RecProcedure(f, x, exp, env1) ->
			let v = eval exp2 env in
			eval exp (extend_env (f, RecProcedure(f, x, exp, env1)) ((extend_env(x, v)) env))
		|_ -> raise(Failure "Type Error")
		)
  
  let run : program -> value
  =fun pgm -> eval pgm empty_env 
