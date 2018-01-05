module ZEXPR =
struct
	exception Error of string
	type id = string
	type expr =
		NUM of int
		|PLUS of expr * expr
		|MINUS of expr * expr
		|MULT of expr * expr
		|DIVIDE of expr * expr
		|MAX of expr list
		|VAR of id
		|LET of id * expr * expr
	type environment = (id * expr) list
	type value = int
	let emptyEnv = []

	let rec eval (env, ex) =
		match ex with
		NUM a -> a
		|PLUS (a, b) -> eval(env, a) + eval(env, b)
		|MINUS (a, b) -> eval(env, a) - eval(env, b)
		|MULT (a, b) -> eval(env, a) * eval(env, b)
		|DIVIDE (a, b) -> eval(env, a) / eval(env, b)
		|MAX [] -> 0
		|MAX (hd::tl) ->
			let hdVal = eval(env, hd) in
			let subMax = eval(env, MAX tl) in
			if(hdVal >= subMax) then hdVal
			else subMax
		|VAR a ->
			let rec findVar (v, vlist) =
				match vlist with
				[] -> raise (Error "variable not initialized")
				|(hd::tl) ->(
					match hd with (x, y) ->
						if((String.compare v x) == 0) then y
						else findVar (v, tl)
				)
			in
			findVar(a, env)
		|LET (i, a, b) -> eval((i, eval(env, a))::env, b)

end


