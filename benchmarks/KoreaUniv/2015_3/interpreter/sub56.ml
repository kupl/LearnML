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
  let extend x v e = (x,v)::e
(*   let extend_f f x e env = (f,RecProcedure (f,x,e,))::env *)
  let apply_env e x = e x

  let rec lookup x e = 
    match e with
    | [] -> raise Not_found
    | (y,v)::tl -> if x = y then v else lookup x tl
    | (f,rp)::tl -> if x = f then rp else lookup x tl
 (*  let rec lookup_f f n e =
    match e with
    | [] -> raise Not_found
    | [f;x;e;env]::tl -> if f = f1 then 
    let env' = extend x n e in
    let env'' = env@env' in
    eval e1 env''
  else lookup_f f n tl *)

  let rec eval_bop = fun bop e1 e2 env ->
  let v1 = eval e1 env in
  let v2 = eval e2 env in 
      (match v1, v2 with
      | Int n1, Int n2 -> Int (bop n1 n2)
      | _ -> raise (Failure "error!"))

  and eval = fun e env -> match e with
  | CONST n -> Int n
  | VAR x -> lookup x env
  | ADD (e1, e2) -> eval_bop (+) e1 e2 env
  | SUB (e1, e2) -> eval_bop (-) e1 e2 env
  | ISZERO e ->
    let v = eval e env in
       (match v with
       | Bool b -> raise (Failure "error")
       | Int n -> if n = 0 then Bool true else Bool false)
  | IF (e1, e2, e3) ->
    let v1 = eval e1 env in 
    (match v1 with
    | Int n -> raise (Failure "error")
    | Bool b -> if b then eval e2 env else eval e3 env)
  | LET (x, e1, e2) ->
    let v1 = eval e1 env in 
    let env' = extend x v1 env in
     eval e2 env'
(*   | LETREC (f, x, e1, env) -> extend_f f x e1 env *)
(*   | CALL (f, n) ->  *)
  let run : program -> value
  =fun pgm -> Int 0 (* TODO *)
