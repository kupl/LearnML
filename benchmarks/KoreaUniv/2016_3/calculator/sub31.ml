
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  type env = int list

  let empty_env = []
  let lookup_env env =
    match env with 
    | [] -> raise (Failure "empty env")
    | hd::tl -> hd
  let extend_env env v = v::env

  let rec eval env exp =
  match exp with
  | X -> lookup_env env
  | INT x -> x
  | ADD (x,y) -> (eval env x) + (eval env y)
  | SUB (x,y) -> (eval env x) - (eval env y)
  | MUL (x,y) -> (eval env x) * (eval env y)
  | DIV (x,y) -> (eval env x) / (eval env y)
  | SIGMA (e1,e2,e3) ->  
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    let rec sigma env (v1,v2,e) = 
      if v1 > v2 then 0
      else if v1 = v2 then let env' = extend_env env v2 in eval env' e
      else let env' = extend_env env v1 in (eval env' e) + sigma env' (v1+1,v2,e) in
    sigma env (v1,v2,e3)

  let rec calculator : exp -> int
  = fun exp -> eval empty_env exp