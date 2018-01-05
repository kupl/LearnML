module type ZEXPR = sig
    
    exception Error of string 
    type id = string
    type expr = 
        | NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr
        | MULT of expr * expr
        | DIVIDE of expr * expr
        | MAX of expr list
        | VAR of id
        | LET of id * expr * expr

    type environment
    type value = int
    
    val emptyEnv: environment
    val eval: environment * expr -> value
    
end

module Zexpr : ZEXPR = struct
    
    exception Error of string 
    type id = string
    type expr = 
        | NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr
        | MULT of expr * expr
        | DIVIDE of expr * expr
        | MAX of expr list
        | VAR of id
        | LET of id * expr * expr

 	type value = int
	type environment = ENV of (id * value) list
	let emptyEnv =
		ENV []
	let rec eval (env, exp) =
		(
		match exp with
		| NUM i -> i
		| PLUS (exp1, exp2) -> eval(env, exp1) + eval(env, exp2)
		| MINUS (exp1, exp2) -> eval(env, exp1) - eval(env, exp2)
		| MULT (exp1, exp2) -> eval(env, exp1) * eval(env, exp2)
		| DIVIDE (exp1, exp2) -> eval(env, exp1) / eval(env, exp2)
		| MAX exp_list ->
			(
			match exp_list with
			| [] -> 0 (* raise (Error "MAX - empty exp list") *)
			| hr::[] -> eval(env, hr)
			| hr::tl ->
				let val_hr = eval(env, hr) in
				let val_max_tl = eval(env, MAX tl) in
				if val_hr > val_max_tl
				then val_hr
				else val_max_tl
			)
		| VAR id_ ->
			let rec get_var_in_env (env_) =
				(
				match env_ with
				| ENV [] -> raise (Error "id not matched")
				| ENV ((id_hr, val_hr)::tl) ->
					if (compare id_hr id_) == 0
					then val_hr
					else get_var_in_env (ENV tl)
				)
			in
			get_var_in_env(env)
		| LET (id_, exp1, exp2) ->
			let env_ =
				(
				match env with
				| ENV ev -> ENV ((id_, eval(env, exp1))::ev)
				)
			in
			eval(env_, exp2)
		)   
end

    
