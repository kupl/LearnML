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
	let emptyEnv = []
	let eval (ienv,iexp) = 
		let rec evals (env,exp) =
			let rec getvalue var nenv = 
				match nenv with
				|(a,b)::tl -> if (a = var) then b
							  else getvalue var tl
				|[]-> raise (Error ("Undefined value") )
			in
			let findmax explist =
				let rec maxmodule nowl nowb =
					match nowl with
					|hd::tl -> (let newb = (evals (env,hd)) in
								if newb > nowb
							   then (maxmodule tl newb)
							   else (maxmodule tl nowb)
							)
						|[] -> nowb
				in
				match explist with
				|a::tl -> maxmodule tl (evals (env,a))
				|[]-> 0
			in
			match exp with
			|NUM s -> s
			|MINUS (a,b) -> (evals (env,a)) - (evals (env,b))
			|PLUS (a,b) -> (evals (env,a)) + (evals(env,b))
			|MULT (a,b) -> (evals (env,a)) * (evals(env,b))
			|DIVIDE (a,b) ->  if (evals(env,b)) = 0 then raise (Error "divide by 0")
							  else (evals(env,a)) / (evals(env,b))
			|MAX lst -> findmax lst
			|VAR x -> getvalue x env
			|LET (s,ep1,ep2) -> 
					(let nenv = ((s,(evals(env,ep1)))::env) in
					 evals(nenv,ep2))
	in
	let result = evals (ienv,iexp) in
	(print_int result);	result
end	
