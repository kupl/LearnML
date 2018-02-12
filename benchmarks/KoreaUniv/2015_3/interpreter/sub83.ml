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
  
  let emptyEnv = fun _ -> raise (Failure "Environment is empty")
  let extendEnv (x,v) e = fun y -> if x = y then v else (e y)
  let applyEnv e x = e x
  

   let rec eval : exp -> env -> value
=fun exp env -> 
match exp with 
| CONST n -> Int n 
| VAR x -> applyEnv env x 
| ADD (e1,e2) -> let v1 = eval e1 env in let v2 = eval e2 env in (match v1,v2 with 
                           | Int n1, Int n2 -> Int (n1 + n2) 
                           | _ -> raise (Failure "Type Error: non-numeric values")) 
| SUB (e1,e2) -> let v1 = eval e1 env in let v2 = eval e2 env in (match v1,v2 with 
                           | Int n1, Int n2 -> Int (n1 - n2) 
                           | _ -> raise (Failure "Type Error: non-numeric values"))
| ISZERO e -> (match eval e env with | Int n when n = 0 -> Bool true | _ -> Bool false) 
| IF (e1,e2,e3) -> (match eval e1 env with 
            | Bool true -> eval e2 env 
            | Bool false -> eval e3 env 
            | _ -> raise (Failure "Type Error: condition must be Bool type")) 
| LET (x,e1,e2) -> let v1 = eval e1 env in eval e2 (extendEnv (x,v1) env)
| PROC (x,e1)->Procedure (x,e1,env)
| CALL (e1,e2)->(match eval e1 env with | Procedure (x,e,env2)->(match eval e2 env with |v2-> eval e (extendEnv (x,v2) env2))
               | RecProcedure (f,x,e,env2)->(match eval e2 env with |v2->eval e (extendEnv (x,v2) (extendEnv (f,RecProcedure (f,x,e,env2)) env2)))  
                   |_->raise (Failure "Type Error: condition must be Procedure") )
| LETREC (f,x,e1,e2) -> eval e2 (extendEnv (f,RecProcedure(f,x,e1,env)) env)

  let run : program -> value
  =fun pgm -> eval pgm emptyEnv
 