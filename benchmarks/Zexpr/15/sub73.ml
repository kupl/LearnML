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
    
    type environment = (id * int) list
    type value = int

    let emptyEnv : environment = []
    let rec eval ((env :environment) ,(ex : expr)) : value = 
	    try 
	    	(match ex with
	    	| NUM(n) -> n
	    	| PLUS(x,y) -> eval(env,x) + eval(env,y)
	    	| MINUS(x,y) -> eval(env,x) - eval(env,y)
	    	| MULT(x,y) -> eval(env,x) * eval(env,y)
	    	| DIVIDE(x,y) -> eval(env,x) / eval(env,y)
	    	| MAX(l) ->
	    		(match l with
	    		| [] -> 0
	    		| h::t -> 
	    			let a = eval(env,h)
	    			and b = eval(env,MAX(t)) in
	    			if List.length t = 0 then a
	    			else max a b)
	    	| VAR(i) -> snd ((List.find (fun x -> fst x = i)) env)	    	
	    	| LET(i,v,ex) -> eval((i,eval(env,v))::env,ex))
	    with Not_found -> raise (Error "FreeVariable")
    
    let print_value v = print_int v

end 