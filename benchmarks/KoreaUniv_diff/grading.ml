type env = (string * int) list

let rec find_env : env -> string -> int
= fun env x ->
	match env with
	| [] -> raise (Failure (x ^ "Not Found"))
	| (y, v)::tl -> if (y = x) then v else find_env tl x

let rec ae_eval : aexp -> (string * int) list -> int
= fun e env ->
	match e with
	| Const n -> n
	| Var x -> find_env env x 
	| Power (x, n) -> 
		if (n = 0) then 1 else (find_env env x) * (ae_eval (Power (x, n-1)) env)
	| Times l ->
		begin 
			match l with
			| [] -> 0
			| [hd] -> ae_eval hd env
			| hd::tl -> (ae_eval hd env) * (ae_eval (Times tl) env)
		end
	| Sum l -> 
		begin
			match l with
			| [] -> 0
			| [hd] -> ae_eval hd env
			| hd::tl -> (ae_eval hd env) + (ae_eval (Sum tl) env)
		end

let grading : (aexp * string) -> env -> int
= fun input env -> ae_eval (diff input) env
