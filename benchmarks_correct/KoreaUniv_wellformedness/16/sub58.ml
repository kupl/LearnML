
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  type env = Env of var list
  let rec envHasIt = fun (env, var) -> match env with
                                | Env(lst) -> (match lst with 
                                                | [] -> false
                                                | hd::tl -> if hd = var then true else
                                                false || envHasIt(Env(tl), var))
  let addVarToEnv: env * var -> env 
  = fun (env, var) -> match env with
        | Env(lst) -> Env(lst@[var])

  let rec envCheck : env * lambda -> bool 
  = fun (env, lambda) -> match lambda with
  | V(v) -> envHasIt(env, v)
  | P(v, e) -> envCheck(addVarToEnv(env, v), e)
  | C(e1, e2) -> envCheck(env, e1) && envCheck(env, e2)
  let check : lambda -> bool
  = fun lambda -> envCheck(Env([]), lambda)
