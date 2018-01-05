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
		 
  let rec eval : environment * expr -> value = fun (env, ex) -> 
		match ex with
    | NUM i -> i
    | PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))  
    | MINUS (e1, e2) ->  (eval (env, e1)) - (eval (env, e2))  
    | MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))  
    | DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))  
    | MAX e -> 
			(match e with
			| [] -> 0
			| fs::[] -> eval (env, fs)
			| fs::sn::tl -> if (eval (env, fs) > eval (env, sn)) then eval (env, MAX (fs::tl))
											else eval (env, MAX (sn::tl)))
    | VAR i -> if (List.exists (fun x -> (fst x) = i) env) then snd (List.find (fun x -> (fst x) = i) env)
							 else raise (Error "FreeVariable") 
    | LET (i, e1, e2) -> eval ((i, eval (env, e1))::env, e2)

  let print_value : value -> unit = fun x -> print_int(x)
		 
end 