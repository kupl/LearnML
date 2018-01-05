module type ZEXPR = 
sig 
	exception Error of string 
	type id = string 
	type expr = 
		|	NUM of int 
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

	type environment = (string * int) list
	type value = int

	let emptyEnv = []
	let rec eval (env, exp) =
		match exp with
		| NUM i -> i
		| PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
		| MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
		| MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
		| DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
		| MAX lst ->
		(
			let first_val =
				match lst with
				| [] -> 0
				| hd::tl -> (eval (env, hd))
				in
				let rec find_max (lst, max_val) =
					(
					match lst with
					| [] -> max_val
					| hd::tl ->
						if ((eval (env, hd)) > max_val) then (find_max (tl, (eval (env, hd))))
						else (find_max (tl, max_val))
					)
					in
					(find_max (lst, first_val))
		)
		| VAR i ->
			let rec find (env, i) =
			match env with
			| [] -> raise (Error "FreeVariable")
			| hd::tl ->
				if ((fst hd) = i) then (snd hd)
				else (find (tl, i))
			in
			(find (env, i))
		| LET (i, exp1, exp2) ->
			let new_env = (i, (eval (env, exp1)))::env in
			(eval (new_env, exp2))


	let print_value v =	print_endline (string_of_int(v))
	(* Implement this module *)
end

