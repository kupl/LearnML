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

    type environment = (id * int) list
    type value = int
    
    let emptyEnv = []
	let int_of_value v = v

	let rec updateEnv env (x, xval) =
		match env with 
			| []				-> [(x, xval)]
			| (name, v) :: tail ->
				if x = name then (x, xval) :: tail
				else (name, v) :: (updateEnv tail (x, xval))

    let rec eval (env, e) =
		match e with
			| NUM n 			-> n
			| PLUS (e1, e2) 	-> (int_of_value (eval (env, e1))) + (int_of_value (eval (env, e2)))
			| MINUS (e1, e2) 	-> (int_of_value (eval (env, e1))) - (int_of_value (eval (env, e2)))
			| MULT (e1, e2) 	-> (int_of_value (eval (env, e1))) * (int_of_value (eval (env, e2)))
			| DIVIDE (e1, e2) 	-> (int_of_value (eval (env, e1))) / (int_of_value (eval (env, e2)))
			| MAX exprlst		->
				(match exprlst with
					| [] 			-> 0
					| head :: []	-> int_of_value (eval (env, head))
					| head :: tail 	-> 
						(let headval = int_of_value (eval (env, head))
						 and maxtail = int_of_value (eval (env, MAX tail)) in
						 	if headval > maxtail then headval else maxtail))

			| VAR x				->
				(match env with
					| []				-> raise (Error "FreeVariable")
					| (name, v) :: tail	->	if x = name then v else (int_of_value (eval (tail, e))))

			| LET (x, e1, e2)	-> 
				(let xval = int_of_value (eval (env, e1)) in
					int_of_value (eval ((updateEnv env (x, xval)), e2)))
end
