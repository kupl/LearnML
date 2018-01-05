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
    let rec eval : environment * expr -> value = fun (env, e) ->
	let rec fndvar (lst, str) = 
		if lst = emptyEnv then raise (Error "FreeVariable")
			else (if (fst (List.hd lst)) = str then (snd (List.hd lst)) else fndvar (List.tl lst, str))
	in
	match (env, e) with
	| (_, NUM a) -> a
	| (env, PLUS (e1,e2)) -> eval (env, e1) + eval (env, e2) 
	| (env, MINUS (e1,e2)) -> eval (env, e1) - eval (env, e2)
	| (env, MULT (e1,e2)) -> eval (env, e1) * eval (env, e2)
	| (env, DIVIDE (e1, e2)) -> eval (env, e1) / eval (env, e2)
	| (env, MAX exlst) -> (if exlst = [] then 0 
					else List.fold_left (fun a b -> (max a (eval (env, b)))) (eval (env, List.hd exlst)) exlst)
	| (env, VAR s) -> fndvar (env, s)
	| (env, LET (a, e1, e2)) -> eval ((a, eval(env, e1)) :: env, e2)
   
     let int_of_value v = match v with
	| a -> a
end

