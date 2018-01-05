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
    type value = int(* TODO *)
    
    let emptyEnv = [](* TODO *)
    let rec eval (env, e) = match e with
		MAX l -> let rec findmax(m, l2) = 
			if (List.length(l2) == 0) then 0 else 
				let max(a, b) = if (a>b) then a else b in
				if (List.length(l2) <= 1) then max(eval(env, (List.nth l2 0)), m)
				else findmax(max(eval(env, (List.nth l2 0)), m), List.tl l2)
			in findmax(eval(env, (List.nth l 0)), List.tl l)
		| LET (a,exp1,exp2) -> eval([(a, eval(env, exp1))] @ env, exp2)  (* env의 range때문에 우선순위 중요함 *)
		| PLUS (exp1,exp2) -> eval(env, exp1) + eval(env, exp2)
		| MINUS (exp1,exp2) -> eval(env, exp1) - eval(env, exp2)
		| MULT (exp1,exp2) -> eval(env, exp1) * eval(env, exp2)
		| DIVIDE (exp1,exp2) -> eval(env, exp1) / eval(env, exp2)  (* if 2nd is 0 then raise Error? *)
		| VAR a -> let rec searchValue ind = 
			if (ind < List.length env) then
				let nval = (List.nth env ind) in
				if (fst(nval) = a) then snd(nval)
				else (searchValue (ind + 1))
			else raise (Error ("FreeVariable"))
			in (searchValue 0)
		| NUM i -> i

    let int_of_value v = v
end

