
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string
	
	let rec checkEnv : lambda * var list -> bool
	= fun (lambda, env) -> match (lambda, env) with
	| (V _, []) -> false
	| (V var, hd::tl) -> if var = hd then true else checkEnv (V var, tl)
	| (P (var, lambda), env) -> checkEnv (lambda, var::env)
	| (C (e1, e2), env) -> checkEnv (e1, env) && checkEnv (e2, env)

  let check : lambda -> bool
  = fun lambda -> checkEnv (lambda, [])
