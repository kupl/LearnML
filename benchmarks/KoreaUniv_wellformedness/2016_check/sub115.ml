
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string
	
	let rec checkEnv : exp * var list -> bool
	= fun (exp, env) -> match (exp, env) with
	| (V _, []) -> false
	| (V var, hd::tl) -> if var = hd then true else checkEnv (V var, tl)
	| (P (var, exp), env) -> checkEnv (exp, var::env)
	| (C (e1, e2), env) -> checkEnv (e1, env) && checkEnv (e2, env)

  let check : exp -> bool
  = fun exp -> checkEnv (exp, [])
