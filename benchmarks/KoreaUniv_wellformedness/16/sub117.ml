
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

	let empty = []
	
	let rec lookup env var =
		match env with
		|[] -> false
		|hd::tl -> if hd = var then true else lookup tl var

	let extend env v = v::env

	let check : exp -> bool
  = fun exp ->
	let rec c_env env ex = 
	match ex with
	|V a -> lookup env a
	|P(a,b) -> let env2 = extend env a in c_env env2 b
	|C(a,b) -> if c_env env a && c_env env b then true else false
	in c_env empty exp
