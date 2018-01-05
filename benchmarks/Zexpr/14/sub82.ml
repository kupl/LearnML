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

	type environment = (string * int) list
	type value = int

	let emptyEnv : environment = []

	let rec get (env, e) =
		let (h, i)::t = env in
			if h = e then i
			else get (t, e)

	let rec contain (env, i) =
		match env with
			| [] -> false
			| (str, v)::tail -> (str = i) || (contain (tail, i))

	let rec eval : environment * expr -> value = fun (env, exp) ->
		match exp with
			| NUM i -> i
			| PLUS (e1, e2) -> eval(env, e1) + eval(env, e2)
		    | MINUS (e1, e2) -> eval(env, e1) - eval(env, e2)
		    | MULT (e1, e2) -> eval(env, e1) * eval(env, e2)
		    | DIVIDE (e1, e2) -> eval(env, e1) / eval(env, e2)
		    | MAX l -> max (env, l)
		    | VAR i -> if (contain (env, i)) then get (env, i)
		    		   else raise (Error "FreeVariable")
		    | LET (i, e1, e2) -> eval((i,(eval (env, e1)))::env, e2)
	and max (env, l) = match l with
			| [] -> 0
			| h::tail when tail = [] -> eval (env, h)
			| h::tail when tail != [] -> if (eval (env, h)) > (max (env, tail)) then eval (env, h)
										else (max (env, tail))	

	let int_of_value : value -> int = fun v -> v
end

