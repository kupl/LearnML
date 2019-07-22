
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

	let rec xfind : (string list * string) -> bool = fun (en, x) ->
	match en with
	| [] -> false
	| hd::tl -> if (hd = x) then true else ((xfind(tl,x)) ||false)

  let rec find : exp * string list -> bool = fun (exp, env) ->
	match exp with
	| V x -> xfind (env, x)
	| P (x, y) -> find(y, x::env)
	| C (x, y) -> find(x, env) && find(y, env)
	
	let rec check : exp -> bool = fun exp -> 
	find (exp,[])	
