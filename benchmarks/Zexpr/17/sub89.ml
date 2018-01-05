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
	  type  id= string
	  type expr =
		|NUM of int
		|PLUS of expr*expr
		|MINUS of expr*expr
		|MULT of expr*expr
		|DIVIDE of expr*expr
		|MAX of expr list
		|VAR of id
		|LET of id*expr*expr
	  type environment = (id*int) list (*example: (x,1),(y,2),...*)
	  type value = int (*result value*)
	  let emptyEnv:environment = []
	  let print_value a= print_int a
	  let rec eval : environment * expr -> value =
		fun(env, exp) ->
		match exp with
		| NUM a -> a
		| PLUS (a,b) -> eval (env,a) + eval (env,b)
		| MINUS (a,b) -> eval(env,a) - eval (env,b)
		| MULT (a,b) -> eval (env,a) * eval (env,b)
		| DIVIDE(a,b) -> eval(env,a) / eval(env,b) (*How about eval(env,b)==0?*)
		| MAX exprlist ->
			(match exprlist with
			| []->0
			| [x]->eval(env,x)
			| x::rest -> (max (eval(env,x)) (eval(env, MAX rest)) )
			)
		| VAR name ->
			(match env with
			| [] -> raise (Error "FreeVariable")
			| [(p,q)] -> if p<>name then raise(Error "FreeVariable") else q
			| (p,q)::rest -> if p<>name then eval(rest, VAR name) else q
			(*duplication check, order check*)
			)
		| LET (name,value,eqt) ->
			eval ( (name,eval(env,value))::env, eqt)

	end 

let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.NUM 1)) 
(*should print 1*)
