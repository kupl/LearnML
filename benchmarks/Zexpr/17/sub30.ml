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
	type environment = (id * value) list

	let emptyEnv : environment = []
    let rec eval : environment * expr -> value = fun (env, exp) ->
		match exp with
		| NUM x -> x
		| PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
		| MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
		| MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
		| DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
		| MAX elist -> 
			(match elist with
			 | [] -> 0
			 | hd::tl ->
				(let rec max = fun elist maxvalue ->
				 match elist with
				 | [] -> maxvalue
				 | hd::tl -> if ((eval (env, hd)) > maxvalue) then max tl (eval (env, hd))
				 			 else max tl maxvalue
				in
				max tl (eval (env, hd))))
		| VAR x ->
			(let rec isvalid = fun var env ->
			 match env with
			 | [] -> raise (Error "FreeVariable")
			 | (id, value)::tl -> if id = var then value
			 					  else isvalid var tl
			 in
			 isvalid x env)
		| LET (x, e1, e2) -> 
			eval ((x, eval (env, e1))::env, e2)

	let print_value : value -> unit = fun x ->
		print_endline (string_of_int x)
end


