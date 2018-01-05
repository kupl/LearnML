module type ZEXPR =
sig
    exception Error of string
    type id = string
    type expr = NUM of int
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
end

module Zexpr : ZEXPR =
struct 
    exception Error of string
    type id = string
    type expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
    | MULT of expr * expr
    | DIVIDE of expr * expr
    | MAX of expr list
    | VAR of id
    | LET of id * expr * expr

    type environment = (id, expr)Hashtbl.t
    type value = int

    let emptyEnv = Hashtbl.create 10

    let rec eval_helper (env, expr) = 
        match expr with
        | NUM int1 -> int1
        | PLUS (expr1, expr2) -> (eval_helper (env, expr1)) + (eval_helper (env, expr2))
        | MINUS (expr1, expr2) -> (eval_helper (env, expr1)) - (eval_helper (env, expr2))
        | MULT (expr1, expr2) -> (eval_helper (env, expr1)) * (eval_helper (env, expr2))
        | DIVIDE (expr1, expr2) -> (eval_helper (env, expr1)) / (eval_helper (env, expr2))
        | MAX exprlist ->
            (match exprlist with
            | [] -> 0
            | [expr1] -> (eval_helper (env, expr1))
            | hd::tl ->
                if (eval_helper (env, hd)) >= (eval_helper (env, (MAX tl))) then (eval_helper (env, hd))
                else (eval_helper (env, (MAX tl)))
			)
        | VAR x -> eval_helper (env, (Hashtbl.find env x))
        | LET (id, expr1, expr2) -> 
			let env2 = Hashtbl.copy env in 
			Hashtbl.replace env2 id (NUM (eval_helper (env, expr1))); eval_helper (env2, expr2)

	let eval (env, expr) =
		let ret = eval_helper (env, expr) in
			print_int (eval_helper (env, expr)); ret
end
