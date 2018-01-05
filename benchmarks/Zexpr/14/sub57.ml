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
	type value

	val emptyEnv : environment
	val eval : environment * expr -> value
	val int_of_value : value -> int
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
	type environment = (string * value) list
	
	let emptyEnv = []
	let rec eval (env, e) =
		match e with
		| NUM n -> n
		| PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
		| MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
		| MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
		| DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
		| MAX [] -> 0
		| MAX (hd :: []) -> eval (env, hd)
		| MAX (hd :: tl) -> max (eval (env, hd)) (eval (env, MAX tl))
		| LET (str, e1, e2) -> eval ((str, eval (env, e1))::env, e2)
		| VAR str ->
			try snd (List.find (fun pair -> ((=) str (fst pair))) env)
			with Not_found -> raise (Error "FreeVariable")
	
	let int_of_value v = v
end
