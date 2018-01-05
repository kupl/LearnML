(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

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
	  
  type value = float
  type environment = (id * value) list
    
  let emptyEnv : environment = []
      
  let rec eval (env, e) =
    match e with
    | NUM a -> float_of_int a
    | PLUS (a, b) -> eval (env, a) +. eval (env, b)
    | MINUS (a, b) -> eval (env, a) -. eval (env, b)
    | MULT (a, b) -> eval (env, a) *. eval (env, b)
    | DIVIDE (a, b) -> eval (env, a) /. eval (env, b)
    | MAX [] -> raise (Error "empty max list\n")
    | MAX (a::b) ->
	 let rec compare m l = 
	   match l with
	   | (hd::tl) -> 
	       let v = eval(env, hd) in
	       if m < v then compare v tl 
	       else compare m tl
	   | _ -> m in
	 compare (eval (env, a)) b
    | VAR a ->       
	let rec checkVariable a env =
	  match env with
	  | (i, _)::tl -> if a = i then true else checkVariable a tl
	  | _ -> false in
	
	let rec getValue a env = 
	  match env with
	  | (i, v)::tl -> if a = i then v else getValue a tl
	  | _ -> raise (Error "NoMatchingValue\n") in 
	
	if (checkVariable a env) then getValue a env
	else raise (Error "FreeVariable\n")
    | LET (id, a, b) ->
	let rec updateEnv ne env =
	  match (ne, env) with
	  | ((i1, v1), (i2, v2)::tl) -> 
	      if i1 = i2 then (i1, v1)::tl
	      else (i2, v2)::(updateEnv ne tl)
	  | (_, []) -> [ne] in
	
	eval ((updateEnv (id, (eval (env, a))) env), b)
	  
  let int_of_value v : int = int_of_float v
end

