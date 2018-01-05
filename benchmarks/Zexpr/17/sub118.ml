(*2016-11690*)
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

  		type environment = (string * int) list
  		type value = int


  		let emptyEnv : environment = []

  		let rec eval : environment * expr -> value = fun (env, exp) ->
  		match exp with
  		| NUM n -> n
  		| PLUS (exp1, exp2) -> (eval (env, exp1)) + (eval (env, exp2))
  		| MINUS (exp1, exp2) -> (eval (env, exp1)) - (eval (env, exp2))
  		| MULT (exp1, exp2) -> (eval (env, exp1)) * (eval (env, exp2))
  		| DIVIDE (exp1, exp2) -> (eval (env, exp1)) / (eval (env, exp2))
  		| MAX exp_list ->
  			let max a b = if a>b then a else b in
  			(	match exp_list with
  				| [] -> 0
  				| single::[] -> eval (env, single)  (*caution!*)
  				| hd::tl -> max (eval (env, hd)) (eval ( env, (MAX tl) ) )
  			)
  		| VAR id ->   					(*List.find : ('a -> bool) -> 'a list -> 'a *)
  			let same_id (x, _) = (id = x) in
  			(
  				try ( snd (List.find same_id env) )
  				with Not_found -> raise (Error "FreeVariable") 
  			)
  		| LET (id, exp1, exp2) ->
  			let eval_exp1 = eval (env, exp1) in
  			let new_env = (id, eval_exp1)::env in
  			eval (new_env, exp2)

  		let print_value : value -> unit = fun x ->
  			print_int x
	end 