module type ZEXPR =
  sig
	exception Error of string
	type id = string
	type expr = NUM of int
			  | PLUS of expr * expr
			  | MINUS of expr * expr
			  | MULT of expr * expr
			  | DIVIDE of expr * expr
			  | MAX of expr list
			  | VAR of id
			  | LET of id * expr * expr
	type environment
	type value
	val emptyEnv: environment
	val eval: environment * expr -> value
  end
  

module Zexpr : ZEXPR =
  struct
	exception Error of string
	type id = string
	type expr = NUM of int
			  | PLUS of expr * expr
			  | MINUS of expr * expr
			  | MULT of expr * expr
			  | DIVIDE of expr * expr
			  | MAX of expr list
			  | VAR of id
			  | LET of id * expr * expr
	type value = int
    type environment = ENV of (id * value) list
	
	let emptyEnv = ENV []
	let eval = fun (o_env, o_exp) ->
		let rec eval_aux = fun (env, exp) ->
			let rec get_env env var =
				match env with
				| ENV [] -> raise (Error ("no environment for " ^ var))
				| ENV((a, b)::tl) -> if a = var then b else get_env (ENV tl) var
			and put_env env var v = 
				match env with
				| ENV [] -> ENV((var, v)::[])
				| ENV((a, b)::tl) ->
					if a = var
					then ENV((a, v)::tl)
					else (
						match (put_env (ENV tl) var v) with
						| ENV l -> ENV((a, b)::l)
					)
			in
			match exp with
			| NUM(x) -> x
			| PLUS(e1, e2) -> eval_aux(env, e1) + eval_aux(env, e2)
			| MINUS(e1, e2) -> eval_aux(env, e1) - eval_aux(env, e2)
			| MULT(e1, e2) -> eval_aux(env, e1) * eval_aux(env, e2)
			| DIVIDE(e1, e2) -> (
				let d = eval_aux(env, e2)
				in
				if d = 0 then raise (Error "Division by zero")
				else eval_aux(env, e1) / d
				)
			| MAX([]) -> 0
			| MAX(hd::tl) -> 
				let hd_val = eval_aux (env, hd)
				and tl_max = eval_aux (env, MAX tl)
				in
				if hd_val > tl_max then hd_val else tl_max
			| VAR(var) -> get_env env var
			| LET(var, e1, e2) -> eval_aux (put_env env var (eval_aux (env, e1)), e2)
		in
		let res = eval_aux (o_env, o_exp)
		in
		print_int res;
		res
  end
