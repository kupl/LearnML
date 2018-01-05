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

		type value = int -> int
		type environment = (id * value) list

		let emptyEnv = []
		let rec eval (env, exp) =
			match exp with
			| NUM i -> fun x -> i
			| PLUS (exp1, exp2) -> fun x -> (eval (env, exp1) x + eval (env, exp2) x)
			| MINUS (exp1, exp2) -> fun x -> (eval (env, exp1) x - eval (env, exp2) x)
			| MULT (exp1, exp2) -> fun x -> (eval (env, exp1) x * eval (env, exp2) x)
			| DIVIDE (exp1, exp2) -> fun x -> (eval (env, exp1) x / eval (env, exp2) x)
			| MAX expl ->  
				let rec subs_elt el x =
			   		match el with
					| [] -> []	
					| h::l -> (eval (env, h) x) :: (subs_elt l x)
				in
				let rec max_list l =
					match l with
					| [] -> 0
					| i::[] -> i
					| i::l' -> max i (max_list l')
				in
				fun x -> max_list (subs_elt expl x)
			| VAR v ->  
				let rec find_val (env, v) = 
					if List.length env = 0 then raise (Error "no variable")
					else if fst (List.hd env) = v then snd (List.hd env)
					else find_val (List.tl env, v)
				in
				find_val(env, v)
			| LET (v, exp1, exp2) ->  
				let xval = (eval (env, exp1) ) in
				eval ((v, xval)::env, exp2) 

		let print_value v = print_int (v 0); 
	end
