
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

	let rec xfind : (string list * string) -> bool = fun (en, x) ->
	match en with
	| [] -> false
	| hd::tl -> if (hd = x) then true else ((xfind(tl,x)) ||false)

  let rec find : lambda * string list -> bool = fun (lambda, env) ->
	match lambda with
	| V x -> xfind (env, x)
	| P (x, y) -> find(y, x::env)
	| C (x, y) -> find(x, env) && find(y, env)
	
	let rec check : lambda -> bool = fun lambda -> 
	find (lambda,[])	
