  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let env = list





  and rec check : exp -> bool
  =fun e -> 
  match e with 
  |V a -> 
  |P (a,b) -> (check b) 
  |C (a,b) -> if ((check a) && (check b)) then true else false 











































end



check(P ("a", C (V "a", P ("b",V "a"))))
check(P ("a", V "a"))



module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let env = [0]

  let rec isthere :  (var*list) -> bool
  = fun (a,b) ->
  match b with
  |hd::tl -> if (hd=a) then true else find(a,tl)
  |_-> false 

  let rec find :  exp -> list -> bool
  =fun a env->
  match e with
  |V a ->isthere(a,env)
  |P (a,b) ->find(b ("a"::env))
  |C (a,b) ->if find(a en)&&find(b en) then true else false


  let check : exp -> bool
  =fun e -> find e [0]
end



(***********************************)
(**            Problem 3          **)
(***********************************)
module Problem3 = struct
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
=fun exp env ->
  match exp with
  | CONST n -> Int n
  | VAR x -> apply_env env x
  | ADD (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1,v2 with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
  | SUB (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1,v2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
  | ISZERO e ->
    (match eval e env with
    | Int n when n = 0 -> Bool true
    | _ -> Bool false)
  | IF (e1,e2,e3) ->
    (match eval e1 env with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise (Failure "Type Error: condition must be Bool type"))
  |LET (x,e1,e2) ->
    let v1 = eval e1 env in
      eval e2 (extend_env (x,v1) env)
  |LETREC (x,y,e1,e2)->
    let v1 = RecProcedure(x,y,e1,env) in
      eval e2 (extend_env (x,v1) env)
  |PROC(x,e)->Procedure(x,e,env)
  |CALL(e1,e2)->
    match (eval e1 env) with
    |Procedure(a,b,c) ->let v1 = (eval e2 env) in eval b (extend_env (a,v1) c)
    |RecProcedure(a,b,c,d) -> let v1 = (eval e2 env) in let v2 = RecProcedure(a,b,c,d) in eval c (extend_env (a,v2) (extend_env (b,v1) d))
    |_ ->raise (Failure "Type Error: non-Procedure values")
  let run : program -> value
  =fun pgm -> eval pgm empty_env
end







(***********************************)
(**            Problem 4          **)
(***********************************)



module Problem4 = struct
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
  





  type value = Int of int | Bool of bool 
             | Procedure of var * exp * env 
             | RecProcedure of var * var * exp * env
  and env = var -> value

  let empty_env = fun _ -> raise (Failure "Environment is empty")
  let extend_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_env e x = e x
  
let rec eval : exp -> env -> value
=fun exp env ->
  match exp with
  | CONST n -> Int n
  | VAR x -> apply_env env x
  | ADD (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1,v2 with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
  | SUB (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1,v2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
  | ISZERO e ->
    (match eval e env with
    | Int n when n = 0 -> Bool true
    | _ -> Bool false)
  | IF (e1,e2,e3) ->
    (match eval e1 env with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise (Failure "Type Error: condition must be Bool type"))
  |LET (x,e1,e2) ->
    let v1 = eval e1 env in
      eval e2 (extend_env (x,v1) env)
  |PROC(x,e)->Procedure(x,e,env)
  |CALL(e1,e2)->
    match (eval e1 env) with
    |Procedure(a,b,c) ->let v1 = (eval e2 env) in eval b (extend_env (a,v1) c)
    |RecProcedure(a,b,c,d) -> let v1 = (eval e2 env) in let v2 = RecProcedure(a,b,c,d) in eval c (extend_env (a,v2) (extend_env (b,v1) d))
    |_ ->raise (Failure "Type Error: non-Procedure values")



  let run : program -> value
  =fun pgm -> eval pgm empty_env




























  let rec split : program -> program
  =fun p ->
  match p with
  |LET(a,b,c) -> c
  |PROC(a,b) -> b
  |_ -> CONST 0
  let salist = ["a"]
  let varilist : program -> list
  =fun p ->


  match p with
  |LET(a,b,c) -> a::salist
  |PROC(a,b) -> a::salist
  |_->salist

  let rec order :(list * string) -> int
  = fun (a,b) ->
  match a with
  |((hd::tl),b) -> if (hd=b) then 0 else (1+order(tl,b))
  |_ -> 0





type value = Int of int | Bool of bool 
             | Procedure of var * exp * env 
             | RecProcedure of var * var * exp * env
  and env = var -> value

  let empty_env = fun _ -> raise (Failure "Environment is empty")
  let extend_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_env e x = e x


 let rec trans : program ->env -> nl_program
 = pgm env ->
   match pgm with
  | CONST a -> NL_CONST a
  | ADD (a,b) -> NL_ADD (translate a,translate b)
  | SUB (a,b) -> NL_SUB (translate a,translate b)
  | ISZERO a -> NL_ISZERO (translate a)
  | IF (a,b,c) -> NL_IF (translate a, translate b, translate c)
  | CALL (a,b) -> NL_CALL (translate a,translate b)
  | LET (a,b,c) ->let v1 = (eval b env)  in NL_LET (translate b (extend_env(a v1) env) ,translate c (extend_env(a v1) env))
  | PROC (a,b) ->NL_PROC (translate b)
  | VAR a -> NL_VAR (order(varilist(split pgm),a))









  let rec translate : program -> nl_program
  =fun pgm -> 
  match pgm with
  | CONST a -> NL_CONST a
  | ADD (a,b) -> NL_ADD (translate a,translate b)
  | SUB (a,b) -> NL_SUB (translate a,translate b)
  | ISZERO a -> NL_ISZERO (translate a)
  | IF (a,b,c) -> NL_IF (translate a, translate b, translate c)
  | CALL (a,b) -> NL_CALL (translate a,translate b)
  | LET (a,b,c) ->NL_LET (translate b,translate c)
  | PROC (a,b) ->NL_PROC (translate b)
  | VAR a -> NL_VAR (order(varilist(split pgm),a))
end








(***********************************)
(**            Problem 5          **)
(***********************************)

module Problem5 = struct
  open Problem4
  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> NL_Int 0 (* TODO *)
end









