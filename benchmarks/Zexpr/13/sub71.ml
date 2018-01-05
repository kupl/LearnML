(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

open Hashtbl
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
    type environment = (id, value) t
    
    let emptyEnv = Hashtbl.create 10
    let rec eval (env, e) = 
			match e with
			| NUM i -> i
			| PLUS (a, b) -> eval(env, a) + eval(env, b)
      | MINUS (a, b) -> eval(env, a) - eval(env, b)
      | MULT (a, b) -> eval(env, a) * eval(env, b)
      | DIVIDE (a, b) -> eval(env, a) / eval(env, b)
      | MAX expList ->
					List.fold_left 
						(fun prevRes curItem -> if (eval(env, curItem) > prevRes) then eval(env, curItem) else prevRes)
						0 expList
(*
				match expList with
				| [] -> 0
				| (hd::tail) -> 
					let hdv = eval(env, hd) in 
					let tailv = eval (MAX(tail)) 
					in if hdv > tailv then hdv else tailv *) 
      | VAR id ->
				begin  
  				try
  					Hashtbl.find env id
  				with Not_found -> raise (Error "FreeVariable")
				end
      | LET (id, exp1, exp2) ->
					let _ = Hashtbl.add env id (eval(env, exp1)) in 
					let res = eval(env, exp2) in
					let _ = Hashtbl.remove env id in res

    let int_of_value v = v
end

