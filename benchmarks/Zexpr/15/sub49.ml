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

	val print_value: value -> unit
end

module Zexpr: ZEXPR =
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

	let rec eval (env, expr): value =
		match expr with
		| NUM i -> i
		| PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
		| MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
		| MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
		| DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))  
		| LET (id, e1, e2) ->
			let push (env, id, e) =
				(id, (eval (env, e)))::env in
				eval ((push (env, id, e1)), e2)
		| VAR id -> 
			let pop (env, id) =
				try
					snd (List.find (fun x -> (fst x) = id) env)
				with
				Not_found -> raise (Error "FreeVariable") in pop (env, id) 
		| MAX li ->
			let rec findmax li env now =
				match li with
				| [] -> now
				| hd::tl ->
					if( now > (eval (env, hd))) then (findmax tl env now)
					else findmax tl env (eval (env, hd)) in 
					match li with
					| [] -> 0
					| hd::tl -> findmax tl env (eval (env, hd))

	let print_value v = print_endline (string_of_int v)

end
