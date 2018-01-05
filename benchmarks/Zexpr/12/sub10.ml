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
    
    val emptyEnv: environment
    val eval: environment * expr -> value
    
end

module Zexpr : ZEXPR = struct
    
    exception Error of string
    exception DividedZero
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
    let eval (env, e) = 
	let rec eval_check (env, e) =
		match e with
		| NUM a -> a
		| PLUS (a, b) -> eval_check (env, a) + eval_check (env, b)
		| MINUS (a, b) -> eval_check (env, a) - eval_check (env, b)
		| MULT (a, b) -> eval_check (env, a) * eval_check (env, b)
		| DIVIDE (a, b) -> if eval_check (env, b) = 0 then raise DividedZero
				   else eval_check (env, a) / eval_check (env, b)
		| MAX [] -> 0
		| MAX (hd::[]) -> eval_check (env, hd)
		| MAX (x::y::tl) ->
			if eval_check (env, x) > eval_check (env, y) then eval_check (env, (MAX (x::tl)))
			else eval_check (env, (MAX (y::tl)))
		| VAR a -> (List.assoc a env)
		| LET (a, ex1, ex2) -> eval_check (((a, (eval_check (env, ex1)))::env), ex2) in
	print_int (eval_check (env, e)); (eval_check (env, e))
end

