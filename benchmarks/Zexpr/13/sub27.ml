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

    type environment = id list * int list
    type value = int
    
		let emptyEnv = ([], [])

		let rec eval (env, e) =
			match e with
			| NUM a -> a
			| PLUS(e1, e2) -> eval(env, e1) + eval(env, e2)
			| MINUS(e1, e2) -> eval(env, e1) - eval(env, e2)
			| MULT(e1, e2) -> eval(env, e1) * eval(env, e2)
			| DIVIDE(e1, e2) -> eval(env, e1) / eval(env, e2)
			| MAX el ->
			(
					match el with
					| [] -> 0
					| hd::tl ->
							let rec aux l max =
							(
								match l with
								| [] -> max
								| h::t -> if max < eval(env, h) then aux t (eval(env, h)) else aux t max
							)
							in aux tl (eval(env, hd))
			)
			| VAR id ->
					let rec aux x l p =
					(
						match l with
						| [] -> raise (Error "FreeVariable")
						| hd::tl -> if hd = x then p else (aux x tl (p + 1))
					)
					in List.nth (snd env) (aux id (fst env) 0)
			| LET (id, e1, e2) -> 
					let newEnv = (id::(fst env), (eval(env, e1))::(snd env)) in
					eval(newEnv, e2)

    let int_of_value v = v
end
