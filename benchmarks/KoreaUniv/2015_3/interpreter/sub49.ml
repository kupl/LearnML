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
type value = 
	Int of int 
	| Bool of bool 
	| Procedure of var * exp * env 
	| RecProcedure of var * var * exp * env 

and env = var -> value 

let empty_env = fun _ -> raise (Failure "Environment is empty")

let extend_env (x,v) e = fun y -> if x = y then v else (e y) 

let apply_env e x = e x 

let run : program -> value =fun pgm ->
 match pgm with 
|VAR x -> apply_env env x
  |ADD (e1,e2) ->  let v1 = run e1 env in
                  let v2 = run e2 env in
  (match v1,v2 with
 |Int n1, Int n2 -> Int (n1 + n2)
 |_-> raise (Failure "ERROR")
 )
 |SUB (e1,e2)-> let v1 = run e1 env in
               let v2 = run e2 env in
 (match v1,v2 with
 |Int n1, Int n2 -> Int (n1 - n2)
 |_-> raise (Failure "ERROR")
 )
 |ISZERO e -> (match run e env with
 |Int n when n = 0 -> Bool true
 |_-> Bool false
 )
 |if (e1,e2,e3) ->
 (match run e1 env with
 |Bool true -> run e2 env
 |Bool false -> run e3 env
 |_-> "ERROR"
 )
 |LET (x,e1,e2) -> let v1= run e1 env in
 run e2 (extend_env (x,v1) env)
 |LETREC (f,x,e1,e2) -> let f2 = run (f ,x,e1,e2) in
 (match f2 with
 |RecProcedure (f3,x2,e3,e4) -> let v2 = run e2 env in
 dd
 |_->raise (Failure "ERROR")
 