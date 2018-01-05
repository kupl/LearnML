

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
		type environment = (id * int) list
		type value = int
		let rec newList a b = match b with
				      | [] ->[]
				      | h::t -> (a, h)::(newList a t)
		let emptyEnv = []
		let rec eval ((env : environment), (exp : expr)) : value =
			match exp with
			| NUM i -> i
			| PLUS (ex1, ex2) -> (eval (env, ex1)) + (eval (env, ex2))
			| MINUS (ex1, ex2) -> (eval (env, ex1)) - (eval (env, ex2))
			| MULT (ex1, ex2) -> (eval (env, ex1)) * (eval (env, ex2))
			| DIVIDE (ex1, ex2) -> (eval (env, ex1)) / (eval (env, ex2))
			| MAX el -> let sortF (a: int) (b: int) : int = if (a<b) then 1
						    			else if (a>b) then -1
						    			else 0 in
				    let a = newList env el in
				    begin match a with
					  | [] -> 0
					  | _ ->  List.hd (List.sort sortF (List.map eval a))
				    end
			| VAR s -> begin try (List.assoc s env) with
					 | Not_found -> (raise (Error "FreeVariable"))
				   end
			| LET (i, ex1, ex2) -> eval (((i, eval (env,ex1))::env), ex2)

		let int_of_value v = v
	end
