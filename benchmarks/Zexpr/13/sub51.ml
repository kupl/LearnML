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

	type value = int
	type pair = string * int
	type environment = NOENV | ENVLST of pair list
    
    let emptyEnv = NOENV					(* environment *)

	let addEnv env (x, v) =					(* environment -> string * int -> environment *)
		match env with
		| NOENV -> ENVLST [(x, v)]
		| ENVLST lst -> ENVLST ((x, v)::lst)
	
	let rec checkEnv env x =				(* environment -> string -> int *)	
		match env with
		| NOENV -> raise (Error "FreeVariable")
		| ENVLST (lst) ->
			match lst with
			| [] -> raise (Error "FreeVariable")
			| l::[] -> if (fst l = x) then (snd l) else raise (Error "FreeVariable")
			| l::t -> if (fst l = x) then (snd l) else checkEnv (ENVLST t) x

	let rec eval (env, e) =					(* environment * expr -> value *)
		match e with
		| NUM n -> n
		| PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
		| MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
		| MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
		| DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
		| MAX (lst) ->
			(
			 match lst with
			 | [] -> 0
			 | l::[] -> eval (env, l)
			 | l::t ->
				(
				 let fst = eval (env, l) and rest = eval (env, MAX (t)) in
				 if (fst >= rest) then fst else rest
				)
			)
		| VAR (var) -> checkEnv env var
		| LET (x,e1,e2) -> eval(addEnv env (x, eval(env, e1)), e2)

	let int_of_value v = v      	  	  (* value -> int *)
end
