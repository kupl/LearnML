
  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
let rec check : lambda -> bool
=fun e -> 
let a = (eval e []) in
match a with
| true -> true
| false -> false


and eval: lambda -> var list -> bool
= fun e env->
match e with
| V x -> lookup x env
| P(x,e1) -> (let v = lookup x env in
		(match v with
		| false -> (eval e1 (extend x env))
		| true -> (eval e1 env)
		))
| C(e1,e2) -> (let a = (eval e1 env) in 
		(match a with 
		| false ->false
		| true -> (eval e2 env)
		))

and lookup: var -> var list -> bool
= fun x env ->
match env with
|[] -> false
| v::tl -> if x = v then true 
	else lookup x tl

and extend: var -> var list -> var list
=fun x env -> x::env
