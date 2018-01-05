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
	type environment = (id * value) list
    
    let emptyEnv = []
	let rec eval (env, e) = 
		match e with
		| NUM x -> x
		| PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
		| MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
		| MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
		| DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
		| MAX l -> getmax (l, 0)
		| VAR x -> getvar (x, env)
		| LET (x, e1, e2) -> eval ([(x, eval (env, e1))] @ env, e2)

	and getmax (l, max) =
		match l with
		| hd::tl -> 
			let val_hd = eval ([], hd) in
			if val_hd > max then getmax (tl, val_hd)
			else getmax (tl, max)
		| [] -> max

	and getvar (str, env) =
		match env with
		| hd::tl ->
			if fst hd = str then snd hd
			else getvar(str, tl)
		| [] -> raise (Error "FreeVariable")

    let int_of_value v = v
end

