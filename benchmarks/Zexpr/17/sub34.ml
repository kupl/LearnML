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

				type value = int
				type environment = (id * value) list  

				let emptyEnv = []
				let print_value v = print_endline (string_of_int v)

				let rec eval (env, e) = 
				match e with
				| NUM n -> n
				| PLUS (a, b) -> eval(env, a) + eval(env, b)
				| MINUS (a, b) -> eval(env, a) - eval(env, b)
				| MULT (a, b) -> eval(env, a) * eval(env, b)
				| DIVIDE (a, b) -> eval(env, a) / eval(env, b)
				| MAX li -> 
								(	match li with
									| [] -> 0
									| [head] -> eval(env, head)
									| (head :: tail) -> if(eval(env, head) < eval(env, MAX tail)) then eval(env, MAX tail) else eval(env, head)
								)
				| VAR i -> 
								(
								 match env with
								 | [] -> (raise (Error "FreeVariable"))
								 | [(hi,hv)] -> if(hi = i) then hv else (raise (Error "FreeVariable"))
								 | ((hi, hv) :: tail) -> if(hi = i) then hv else eval(tail, VAR i)
								)
				| LET (i, e1, e2) -> eval(((i,eval(env,e1))::env), e2)

    end
