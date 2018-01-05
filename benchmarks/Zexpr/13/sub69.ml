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
    val makeEnv : (id * int * environment) -> environment

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
    type value = int
    
    let emptyEnv = []
    let makeEnv (name, value, env) = 
		let rec srch_lst(a, b, k) = match a with
		| (u,v)::t -> if String.compare u b=0 then (u,k)::t else (u,v)::srch_lst(t, b, k)
		| [] -> [(name, value)]
		in srch_lst(env, name, value)
    let rec eval (env, e) = match e with
	| NUM e -> e
	| PLUS (a, b) -> eval(env,a) + eval(env,b)
	| MINUS (a, b) -> eval(env,a) - eval(env,b)
	| MULT (a, b) -> eval(env,a) * eval(env,b)
	| DIVIDE (a, b) -> eval(env,a) / eval(env,b)
	| MAX (e) ->
		let rec max_lst (arg2) =
		if List.length(arg2) = 0 then 0
		else
			if eval(env, List.hd(arg2)) > (max_lst(List.tl(arg2))) then
			eval(env, List.hd(arg2))
			else
				max_lst(List.tl(arg2))
		in 
		max_lst(e)
	| VAR (e) -> 
		let rec find_lst(a, b) = match a with
		| (u,v)::t -> if String.compare u b=0 then v else find_lst(t, b)
		| [] -> raise(Error("FreeVariable"))
		in find_lst(env, e)
	| LET (a, b, c) -> 
		eval(makeEnv(a,eval(env, b),env), c)

    let int_of_value v = (v:int)

end
