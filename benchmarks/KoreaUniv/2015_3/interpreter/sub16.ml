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
  
  let rec eval : exp -> env -> value
=fun exp env -> match exp with
  CONST i -> Int i
| VAR v -> apply_env env v
| ADD (e1,e2) ->
  let v1 = eval e1 env in
  let v2 = eval e2 env in
    (match v1,v2 with
    | Int n1, Int n2 -> Int (n1+n2)
    | _ -> raise (Failure "Type Error at ADD"))
| SUB (e1,e2) ->
  let v1 = eval e1 env in
  let v2 = eval e2 env in
    (match v1,v2 with
    | Int n1, Int n2 -> Int (n1-n2)
    | _ -> raise (Failure "Type Error at ADD"))
| ISZERO e ->let e1=eval e env in
  (match e1 with
  | Int x -> if x=0 then Bool true else Bool false
  | _ -> raise (Failure "Type Error at ISZERO"))
| IF (e1,e2,e3) -> let v1=eval e1 env in
  (match v1 with
  | Bool b -> if b=true then eval e2 env else eval e3 env
  | _ -> raise (Failure "Type Error at IF"))
| LET (v1,e1,e2) -> let v2= eval e1 env in eval e2 (extend_env (v1,v2) env)
| LETREC (v1,v2,e1,e2) -> let v3=RecProcedure(v1,v2,e1,env) in eval e2 (extend_env (v1,v3) env)
| PROC (v,e) -> Procedure (v,e,env)
| CALL (e1,e2) -> (match eval e1 env with
  | Procedure(x,e,env2) -> let v=eval e2 env in eval e (extend_env (x,v) env2)
  | RecProcedure(x1,x2,e,env2) -> let v=eval e2 env in
    let v3=RecProcedure(x1,x2,e,env2) in
      let env3=extend_env (x1,v3) env2 in eval e (extend_env (x2,v) env3)
  | _ -> raise (Failure "Type Error at CALL"))


let rec run : program -> value
=fun pgm -> eval pgm empty_env;;
