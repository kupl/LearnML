(*********************************
 ** PL::HW[02].Problem[06]      **
 **                             **
 ** Mod. Init: 2014-09-27 22:28 **
 ** Mod. Fin.: 2014-09-30 19:54 **
 **                             **
 ** Writ. by : CMS              **
 *********************************)

module type ZEXPR = 
sig
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
	type value

	val emptyEnv: environment
	val eval: environment * expr -> value

	val int_of_value: value -> int
end

module Zexpr : ZEXPR = 
struct
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
	type environment = (id * value) list

	let emptyEnv = []
	let rec eval (env, expr_in) = match expr_in with

		(* Base case *)
		| NUM n -> n

		(* Arithmetic Operations *)
		| PLUS(e1, e2) -> eval(env, e1) + eval(env, e2)
		| MINUS(e1, e2) -> eval(env, e1) - eval(env, e2)
		| MULT(e1, e2) -> eval(env, e1) * eval(env, e2)
		| DIVIDE(e1, e2) -> eval(env, e1) / eval(env, e2)
		| MAX expr_list -> 
		(
			match expr_list with
			| [] -> 0
			| e::[] -> eval(env, e)
			| e::el -> 
				let max_of_el = eval(env, MAX el) in
				let value_of_e = eval(env, e) in
					if value_of_e > max_of_el then value_of_e else max_of_el
		)

		(* Function that check the variable [name] is available *)
		| VAR name -> 
		(
			try
				List.assoc name env
			with Not_found -> raise (Error "FreeVariable")
		)

		(* Function that
			declare an variable [var_name],
			assign [var_value], and
			designate scope as [expr_val_used] *)
		| LET(var_name, var_value_expr, val_valid_expr) -> 
			let v = eval(env, var_value_expr) in
				eval((var_name, v)::env, val_valid_expr)

	let int_of_value n = n
end

