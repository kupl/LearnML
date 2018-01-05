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
;;

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
		type value = INT of int
		let int_of_value input = 
			match input with
			INT value -> value
		
		module MyMap = Map.Make(String)
		let emptyEnv = MyMap.empty 
 		type environment = int MyMap.t
		let rec eval (env, formula) = 
			match formula with
			  NUM value -> INT value
			| PLUS (first, second) -> INT ((int_of_value (eval (env, first))) + (int_of_value (eval (env, second))))
			| MINUS (first, second) -> INT ((int_of_value (eval (env, first))) - (int_of_value (eval (env, second))))
			| MULT (first, second) -> INT ((int_of_value (eval (env, first))) * (int_of_value (eval (env, second))))
			| DIVIDE (first, second) -> INT ((int_of_value (eval (env, first))) / (int_of_value (eval (env, second))))
			| MAX int_list -> let rec doMax (now, left) =
				match left with
				  [] -> INT now
				| l :: r -> if now < (int_of_value (eval (env, l))) then doMax (int_of_value (eval (env, l)), r)
						else doMax (now, r)
				in
				doMax (0, int_list)
			| VAR first -> (try 
					INT (MyMap.find first env) 
					with Not_found -> raise (Error ("FreeVariable"))) 
			| LET (first, second, third) -> eval((MyMap.add first (int_of_value (eval (env, second))) env) , third)
	end
;;










