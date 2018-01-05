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

		type value = VAL of int
		
		type environment = 
		| EMPTY 
		| E of id * value * environment 
		
		let emptyEnv = EMPTY
		
		let int_of_value = function(VAL i) -> i

		let rec eval (env, exp) =
			match exp with
			| NUM i -> VAL i
			| PLUS (e1,e2) -> VAL (int_of_value (eval (env , e1)) + int_of_value (eval (env , e2)))
			| MINUS (e1,e2) -> VAL (int_of_value (eval (env , e1)) - int_of_value (eval (env , e2)))
			| MULT (e1,e2) -> VAL (int_of_value (eval (env , e1)) * int_of_value (eval (env , e2)))
			| DIVIDE (e1,e2) -> VAL (int_of_value (eval (env , e1)) / int_of_value (eval (env , e2)))
			| MAX e_list -> let i_list = List.map (fun x -> int_of_value (eval (env , x))) e_list in
							let rec find_M li fst = (match li with
													 | [] -> fst
													 | hd::tl -> if fst > hd then find_M tl fst
													 			 else find_M tl hd) in
							(match e_list with
							 | [] -> VAL 0
							 | _ -> VAL (find_M (List.tl i_list) (List.hd i_list)))
			| VAR x -> (match env with
						| EMPTY -> raise (Error "FreeVariable")
						| E (id, value, env_valid) -> if id = x then value else eval (env_valid, exp))
			| LET (x, exp1, exp2) -> eval (E (x, eval (env, exp1), env) , exp2)

end
	

