(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ackr)
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

    type environment = (id * int) list
    type value = (environment * int)
    
    let emptyEnv = []

	let rec findvar env x =
		match env with
		| [] -> raise (Error "FreeVariable")
		| hd::tl ->
			if (fst hd) = x then (snd hd)
			else findvar tl x

    let rec eval (env, e) =
		match e with
		| NUM i -> (env, i)
		| PLUS (e1, e2) -> (env, (snd (eval (env, e1))) + (snd (eval (env, e2))))
		| MINUS (e1, e2) -> (env, (snd (eval (env, e1))) - (snd (eval (env, e2))))
		| MULT (e1, e2) -> (env, (snd (eval (env, e1))) * (snd (eval (env, e2))))
		| DIVIDE (e1, e2) -> (env, (snd (eval (env, e1))) / (snd (eval (env, e2))))
		| MAX lst ->
		(
			match lst with
			| [] -> (env, 0)
			| hd::tl ->
				if tl = [] then (env, (snd (eval (env, hd))))
				else if ((eval (env, hd)) > (eval (env, (MAX tl)))) then (env, (snd (eval (env, hd))))
				else eval (env, (MAX tl))
		)
		| VAR x -> (env, findvar env x) 
		| LET (x, e1, e2) -> 
			let newenv = ([(x, (snd (eval (env, e1))))] @ env) in
			(newenv, (snd (eval (newenv, e2))))
    let int_of_value v = (snd v)
end

