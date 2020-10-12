type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> test lam []
and test : lambda -> var list -> bool
= fun lam env ->
	match lam with
	| V v -> find_env v env
	| P (v, l) -> test l (v::env)
	| C (l1, l2) -> (test l1 env)&&(test l2 env)
and find_env : var -> var list -> bool
= fun v env ->
	match env with
	| [] -> false
	| h::t -> if (h=v) then true else (find_env v t);;

