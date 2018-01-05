
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

module Zexpr = 
struct
	exception Error of string
	type id = string
	type expr =
		NUM of int
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
	let rec eval (env,exp) : value =
		match exp with
		| NUM n -> n
		| PLUS(e1,e2) -> (eval (env,e1)) + (eval (env,e2))
		| MINUS(e1,e2) -> (eval (env,e1)) - (eval (env,e2))
		| MULT(e1,e2) -> (eval (env,e1)) * (eval (env,e2))
		| DIVIDE(e1,e2) -> (eval (env,e1)) / (eval (env,e2)) 
		| MAX elist ->	(match elist with
						| head::tail -> 	if tail = [] then eval(env,head) 
											else (max (eval(env,head)) (eval (env, MAX tail)))
						| [] -> 0)
						
		| VAR x ->	(let isTheIdValue theID = function (str, n) ->	(str = theID) in
					try
						(snd (List.find (isTheIdValue x) env))
					with Not_found -> raise (Error "FreeVariable"))
					
		| LET (x, e1, e2) ->	let envElt = (x, eval(env, e1)) in
								eval(envElt::env, e2)
							
	let int_of_value = function n -> n
end