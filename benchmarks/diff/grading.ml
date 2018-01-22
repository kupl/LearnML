type env = (string * int) list

let rec find_env : env -> string -> int
= fun env x ->
	match env with
	| [] -> raise (Failure (x ^ "Not Found"))
	| (y, v)::tl -> if (y = x) then v else find_env tl x

let rec ae_eval : ae -> (string * int) list -> int
= fun e env ->
	match e with
	| CONST n -> n
	| VAR x -> find_env env x 
	| POWER (x, n) -> 
		if (n = 0) then 1 else (find_env env x) * (ae_eval (POWER (x, n-1)) env)
	| TIMES l ->
		begin 
			match l with
			| [] -> raise (Failure "Invalid")
			| [hd] -> ae_eval hd env
			| hd::tl -> (ae_eval hd env) * (ae_eval (TIMES tl) env)
		end
	| SUM l -> 
		begin
			match l with
			| [] -> raise (Failure "Invalid")
			| [hd] -> ae_eval hd env
			| hd::tl -> (ae_eval hd env) + (ae_eval (SUM tl) env)
		end

let grading : (ae * string) -> env -> int
= fun input env -> ae_eval (diff input) env
