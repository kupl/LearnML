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

		type environment = (id * int) list

		type value = int

		let emptyEnv : environment = []

		let rec eval : environment * expr -> value = fun (env,exp) ->
			match exp with
			|NUM num -> num
			|PLUS (exp1,exp2) -> (eval(env,exp1))+(eval(env,exp2))
			|MINUS (exp1,exp2) -> (eval(env,exp1))-(eval(env,exp2))
			|MULT (exp1,exp2) -> (eval(env,exp1))*(eval(env,exp2))
			|DIVIDE (exp1,exp2) ->  
				if (eval (env,exp2)) = 0 then
					raise(Error "Division_by_zero")
				else 
					( (eval (env,exp1) )/( eval (env,exp2)) )
			|MAX l ->
				(match l with
				|head::tail ->
					(match tail with
					|hd::tl ->
							if ((eval (env,head)) < (eval (env,hd))) then 
								eval (env, (MAX (hd::tl)))
							else 
								eval (env, (MAX (head::tl)))
					|[] -> eval (env, head)
					)
				|[] -> 0
				)
			|VAR i -> 
				(match env with
				|(i1,a)::tail ->
					(if (i = i1) then
						a
					else
						eval (tail, VAR i)
					)
				|emptyEnv -> raise(Error "FreeVariable")
				)
			|LET (i,exp1,exp2) -> eval( ( (i, (eval(env,exp1) ))::env), exp2)

		let print_value : value -> unit = fun v -> 
			let prt = print_int v
			in 
			prt;
			print_newline()

	end
