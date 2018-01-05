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
    

	let rec findVarEnv(env, var) = 
		match env with 
		| [] -> raise(Error "No Value in Environment")
		| hd::tl ->
			if fst(hd) = var then snd(hd)
			else findVarEnv(tl, var)
	let rec intEval(env, e) = 
		match e with 
		| NUM n -> n
		| PLUS(exp1, exp2) -> intEval(env, exp1) + intEval(env, exp2)
		| MINUS(exp1, exp2) -> intEval(env, exp1) - intEval(env, exp2)
		| MULT(exp1, exp2) -> intEval(env, exp1) * intEval(env, exp2)
		| DIVIDE(exp1, exp2) -> 
			if intEval(env, exp2) = 0 then raise(Error "Divide by zero")
			else intEval(env, exp1) / intEval(env, exp2)
		| VAR var -> findVarEnv(env, var)
		| LET(var, exp1, exp2) -> intEval(((var, intEval(env, exp1))::env), exp2)
		| MAX expl ->
			match expl with 
			| [] -> 0
			| ex::[] -> intEval(env, ex)
			| ex::exl -> 
				if intEval(env, ex) < intEval(env, MAX(exl)) then intEval(env, MAX(exl))
				else intEval(env, ex)


    let emptyEnv = []
    let eval (env, e) = snd(print_int(intEval(env, e)), intEval(env, e))
end

    
