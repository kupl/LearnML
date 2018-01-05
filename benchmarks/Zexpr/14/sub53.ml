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

module Zexpr : ZEXPR = struct
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
	type value = int
	type environment = (id*value) list

	
	let emptyEnv = []
	let rec eval (env,expr) = 
		match expr with
			| NUM i -> i
			| PLUS (e1,e2) -> (eval (env,e1))+(eval (env,e2))
			| MINUS (e1,e2) -> (eval (env,e1))-(eval (env,e2))
			| MULT (e1,e2) -> (eval (env,e1))*(eval (env,e2))
			| DIVIDE (e1,e2) -> (eval (env,e1))/(eval (env,e2))
			| MAX eList -> let rec max (v,l) = 
							if l = [] then v
							else (if v >= (List.hd l) then max (v,List.tl l)
									else max (List.hd l,List.tl l))
							in
							let valueList = (List.map (fun e -> eval (env,e)) eList) in
							if eList = [] then 0 else max (List.hd valueList,List.tl valueList)
			| VAR id -> if List.mem id (List.map fst env) then snd (List.find (fun (a,b) -> (a=id)) env)
						else raise (Error "FreeVariable")
			| LET (id,num,e) -> eval ((id,eval (env,num))::env,e)

	let int_of_value v = v
end
