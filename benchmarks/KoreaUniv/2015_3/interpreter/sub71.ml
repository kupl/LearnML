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


let rec eval_bop = fun op exp1 exp2 env ->
			let value1 = eval exp1 env in
			let value2 = eval exp2 env in
				(match value1,value2 with
				 |Int int1, Int int2 -> Int (op int1 int2)
				 |_->raise (Failure "NOT_MATCH")
				 )


and eval = fun exp env ->
		 match exp with
			|CONST n -> Int n
			|VAR x -> apply_env env x
			|ADD (exp1,exp2) -> eval_bop (+) exp1 exp2 env
			|SUB (exp1,exp2) -> eval_bop (-) exp1 exp2 env
			|ISZERO exp -> (let value = eval exp env in
								match value with
								|Int n -> if n=0 then Bool true else Bool false
								|_->raise(Failure "TYPE_ERROR")
					)
			|IF (exp1,exp2,exp3)->
								(match eval exp1 env with
								 |Bool true -> eval exp2 env
								 |Bool false-> eval exp3 env
								 |_->raise (Failure "TYPE_ERROR")
								 )
			|LET (var,exp1,exp2)->
								let value1 = eval exp1 env in
									eval exp2 (extend_env (var,value1) env )
			|LETREC (var1,var2,exp1,exp2)->
								eval exp2 (extend_env (var1, RecProcedure(var1,var2,exp1,env) ) env )
			|PROC (var,exp)-> Procedure (var,exp,env)	
			|CALL (exp1,exp2)-> (match eval exp1 env with
								|RecProcedure(var1,var2,pexp1,envprime)->
									let v = eval exp2 env in
										eval pexp1( extend_env (var2,v) (extend_env (var1,RecProcedure(var1,var2,pexp1,envprime)) envprime)  )
								
								|Procedure(var,pexp1,envprime)->	
									let v = eval exp2 env in
										eval pexp1(extend_env (var,v) env)
								|_->raise (Failure "TYPE_ERROR")

								)

let run : program -> value
=fun pgm -> eval pgm empty_env
