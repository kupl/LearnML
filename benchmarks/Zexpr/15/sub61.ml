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
  type environment = ( id * value ) list

  let rec findID i l = match l with [] -> 1
    					| (h_id, h_val)::t -> if ((compare i h_id) = 0) then 1
    					         			 else (findID i t)+1
  let rec deleteID i l = match l with [] -> []
  						| (h_id, h_val)::t -> if ((compare i h_id) = 0) then t
  											else [(h_id, h_val)]@(deleteID i t)

  let emptyEnv : environment = []
  let rec eval ( env, ex ) = match ex with
  	| NUM i -> i
    | PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
    | MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
    | MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
    | DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
    | MAX l -> (match l with [] -> 0 
    					| h::t -> if (eval (env, h)) > (eval (env, MAX t)) then (eval (env, h))
    					  		else ( match t with [] -> (eval (env, h))
    					  				| hh::tt -> (eval (env, MAX t))
    					  			)
    			)
    | VAR i -> (match env with [] -> raise (Error "FreeVariable")
    			| (h_id, h_val)::t -> if ((compare i h_id) = 0) then h_val
    								else eval (t, ex)
    		 	)
    | LET (i, e1, e2) -> if (findID i env) > (List.length env) then eval ((i, (eval (env, e1)))::env, e2)
    					else eval ((i, (eval (env, e1)))::(deleteID i env), e2)

  let print_value v = print_int(v)
end

