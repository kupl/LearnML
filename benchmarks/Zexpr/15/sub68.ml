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
	val emptyEnv : environment
	val eval : environment * expr -> value
	val print_value : value -> unit
end;;

module Zexpr =
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
		type environment = (id * value) list
		let emptyEnv = []
		let rec eval (env, exp) = 
			match exp with
			| NUM a -> a
			| PLUS (a, b) -> eval(env, a) + eval(env, b)
			| MINUS (a, b) -> eval(env, a) - eval(env, b)
			| MULT (a, b) -> eval(env, a) * eval(env, b)
			| DIVIDE (a, b) -> eval(env, a) / eval(env, b)
			| MAX a -> if a = [] then 0 else max(env, a)
			| VAR a -> var(env, a)
			| LET (a, e1, e2) -> letvar(env, a, e1, e2)
		and max(env, a) =
			let maxtwo a b = if a > b then a else b in
			match a with
			| [] -> 0
			| [element] -> eval(env, element)
			| head::tail -> maxtwo (eval(env, head)) (max(env, tail))
		and var(env, a) = 
			match env with
			| [] -> raise (Error "Free Variable")
			| head::tail -> let (hid, hvalue) = head in
							if hid = a then hvalue
							else var(tail, a)
		and letvar(env, a, e1, e2) =
			let newenv = (a, eval(env, e1))::env in
			eval (newenv, e2)

		let print_value a = print_endline(string_of_int a)
	end;;
