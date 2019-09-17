
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  type env = var list

  let empty_env = []

  let rec lookup_env env v = 
    match env with
    | [] -> false
    | hd::tl -> if hd = v then true else lookup_env tl v

  let extend_env env var = var::env

  let check : lambda -> bool
  = fun lambda -> 
  let rec check_env env e = 
  match e with
  | V x -> lookup_env env x
  | P (x,y) -> let env' = extend_env env x in check_env env' y
  | C (x,y) -> if check_env env x && check_env env y then true else false
  in check_env empty_env lambda
