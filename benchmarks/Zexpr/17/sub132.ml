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

	type value = int
	
	type environment = (id * value) list
	let emptyEnv : environment = []

	let rec eval ((env, ex) : environment * expr) : value =
		match ex with
		| NUM n -> n
		| PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
		| MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
		| MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
		| DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
		| MAX el ->
			(let rec sub_max (el : expr list) : value =
				match el with
				| [] -> 0
				| hd::[] -> eval (env, hd)
				| hd::tl ->
					(let val_hd : value = eval (env, hd) in
						let max_tl : value = (sub_max tl) in
							if val_hd > max_tl then val_hd else max_tl)
			 in sub_max el)
		| VAR i -> 
			(try snd (List.find (fun t -> fst t = i) env) with
				Not_found -> raise (Error "FreeVariable"))
		| LET (i, e1, e2) -> 
			eval ((i, eval(env, e1))::env, e2)

	let print_value (x : value) : unit = print_int x
end 
