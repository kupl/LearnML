(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)
open List
open Map

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

	module IdMap = Map.Make(String)
    type environment = expr IdMap.t
    type value = int
    
    let emptyEnv = IdMap.empty

	let int_of_value : value -> int =
	    fun v -> v
    let rec eval : environment * expr -> value=
        fun (env, e) ->
		    match e with 
			|NUM n -> n
			|PLUS (e1,e2) -> (int_of_value (eval (env,e1)))+(int_of_value (eval (env,e2)))
			|MINUS (e1,e2) -> (int_of_value (eval (env,e1)))-(int_of_value (eval (env,e2)))
			|MULT (e1,e2) -> (int_of_value (eval (env,e1)))*(int_of_value (eval (env,e2)))
			|DIVIDE (e1,e2) -> (int_of_value (eval (env,e1)))/(int_of_value (eval (env,e2)))
	        |MAX [] -> 0
			|MAX lst -> 
			    let int_lst= List.map (function x -> int_of_value (eval (env,x))) lst in
				List.fold_left (fun x y -> if x>=y then x else y) (List.hd int_lst) (List.tl int_lst)
	        |VAR i -> 
			    if (IdMap.mem i env) then (eval (env,(IdMap.find i env)))
				else raise (Error "FreeVariable")
			|LET (i,v,e) -> eval ((IdMap.add i v env),e)
			    
			    
    (*let int_of_value : value -> int =
        fun v -> v*)
end

