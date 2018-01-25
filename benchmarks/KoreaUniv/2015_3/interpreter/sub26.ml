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
let apply_env (x,e) = e x


let rec eval : program * env -> value
=fun (pgm,env) ->
match pgm with
CONST a -> Int a
|VAR v -> apply_env (v,env)
|ADD (ex1,ex2) -> 
let v1= eval (ex1,env) in
let v2= eval (ex2,env) in
(
  match v1,v2 with
  | Int n1, Int n2 -> Int (n1+n2)
  |_ -> raise (Failure "Environment is empty")
) 
|SUB (ex1,ex2) ->
let v1= eval (ex1,env) in
let v2= eval (ex2,env) in
(
  match v1,v2 with
  | Int n1, Int n2 -> Int (n1-n2)
  |_ -> raise (Failure "Environment is empty")
) 
|ISZERO (ex) -> 
(match eval(ex,env) with
  Int n -> if(n=0) then Bool true else Bool false
  |_-> raise (Failure "syntax error")
)
|IF (ex1,ex2,ex3) -> 
(match eval(ex1,env) with
  Bool b-> if(b=true) then eval(ex2,env) else eval(ex3,env)
  |_-> raise (Failure "syntax error")
)
|LET (v,ex1,ex2) -> 
  let v1 = eval (ex1,env) in
      eval (ex2,(extend_env (v,v1) env))
|LETREC (v1,v2,ex1,ex2) -> 
  eval (ex2,(extend_env (v1,RecProcedure (v1,v2,ex1,env)) env))
|PROC (v,ex) -> Procedure (v,ex,env)
|CALL (ex1,ex2) ->
  match eval(ex1,env) with
  RecProcedure (f,x,e,env2) -> 
  (
    let v=eval (ex2,env) in
    eval(e, extend_env (f,RecProcedure (f,x,e,env2)) (extend_env (x,v) env2))
  )
  |Procedure (x,e,env2)->
  (
    let v=eval (ex2,env) in
    eval(e, extend_env (x,v) env2)
  )
  |_ -> raise (Failure "Can't find") 

let rec run : program -> value
=fun pgm -> (* TODO *)
eval(pgm, empty_env)
