	module type ZEXPR =
	sig
		exception Error of string
		type id = string
		type expr =
				|NUM of int
				|PLUS of expr * expr
				|MINUS of expr * expr
				|MULT of expr * expr
				|DIVIDE of expr * expr
				|MAX of expr list
				|VAR of id
				|LET of id * expr * expr
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
				|NUM of int
				|PLUS of expr * expr
			        |MINUS of expr * expr
				|MULT of expr * expr
                        |DIVIDE of expr * expr
                        |MAX of expr list
                        |VAR of id
                        |LET of id * expr * expr
	type value = int
	type environment = ( id * value ) list

	let rec env_finder : environment * id -> value = fun(env_, id_) ->
        	match env_ with
            		[] -> raise( Error("FreeVariable") )
           	   |hd::tail -> (
				 if ( fst hd = id_ ) then (snd hd)
				 else env_finder(tail, id_)
				)

	let emptyEnv : environment = []
	let rec eval : environment * expr -> value = fun(env,expr_) ->
			match expr_ with
				 NUM(n) -> n
				|PLUS(expr1, expr2) -> eval(env,expr1) + eval(env,expr2)
				|MINUS(expr1, expr2) -> eval(env,expr1) -  eval(env,expr2)
 				|MULT(expr1, expr2) -> eval(env,expr1) * eval(env,expr2)
				|DIVIDE(expr1, expr2) -> eval(env,expr1) / eval(env,expr2)
				|VAR(id_) -> env_finder(env,id_)
				|LET(id_,expr1,expr2) -> eval( (id_,eval(env,expr1))::env, expr2 ) 
				|MAX(exprs) -> ( 
					match exprs with
					  hd_expr::[] -> eval(env,hd_expr)
					 |hd_expr::tail_exprs -> (
						if ( eval(env,hd_expr) >= eval(env,MAX(tail_exprs))) then (eval(env,hd_expr))
						else(eval(env,MAX(tail_exprs)))
								 )
					 |[] -> 0 (* raise(Error("Max on Empty List")) *)
				 )
	let  print_value : value -> unit = fun(value_) -> print_endline(string_of_int(value_))


end
