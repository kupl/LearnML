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

		type environment = (string * expr) list
		type value = int

		let emptyEnv : environment = []
		let rec eval (v, e) : value =
			match e with
			  NUM i -> i
			| PLUS (e1, e2) -> eval (v, e1) + eval (v, e2)
			| MINUS (e1, e2) -> eval (v, e1) - eval (v, e2)
			| MULT (e1, e2) -> eval (v, e1) * eval (v, e2)
			| DIVIDE (e1, e2) -> 
				if(eval (v, e2) = 0) then raise (Error "Divided By ZERO")
				else eval (v, e1) / eval (v, e2)
			| MAX l ->
				(match l with
				  [] -> 0
				| h::[] -> eval(v, h)
				| h::t::l -> 
					if(eval (v, h) > eval (v, t)) then eval (v, MAX (h::l))
					else eval (v, MAX (t::l)))
			| VAR i ->
				(match v with
				  [] -> raise (Error "FreeVarable")
				| (x, e)::t ->
					if(x = i) then eval (t, e)
					else eval (t, VAR i))
			| LET(x, e1, e2) -> eval ((x,e1)::v, e2)

		let print_value v : unit = print_endline (string_of_int v)
	end 
