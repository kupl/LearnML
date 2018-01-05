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

	let rec env_value l i =
	  match l with
	  | [] -> (raise (Error "FreeVariable"))
	  | (hi, hv)::tl ->
        if (hi=i) then hv
        else (env_value tl i)

	let rec new_var l i v =
	  match l with
	  | [] -> (i, v)::[]
	  | (hi, hv)::tl ->
	    if (hi=i) then (i, v)::tl
		else (hi, hv)::(new_var tl i v)

    let rec eval (env, e) = 
	  match e with
	  | NUM a -> a
	  | PLUS (e1, e2) -> ((eval (env, e1)) + (eval (env, e2)))
	  | MINUS (e1, e2) -> ((eval (env, e1)) - (eval (env, e2)))
	  | MULT (e1, e2) -> ((eval (env, e1)) * (eval (env, e2)))
	  | DIVIDE (e1, e2) -> ((eval (env, e1)) / (eval (env, e2)))
	  | MAX l -> 
	    (match l with
		| [] -> 0
		| hd::tl ->
  		  (let k1 = (eval (env, hd)) in let k2 = (eval (env, MAX tl)) in
		    if (k1 > k2) then k1
		    else k2))
		  
	  | VAR i -> (env_value env i)
	  | LET (i, e1, e2) -> (eval ((new_var env i (eval (env, e1))), e2))
	    

    let int_of_value v = v

end


