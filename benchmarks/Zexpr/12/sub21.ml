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
    
    let emptyEnv = []
    let eval (env, e) =
	let rec finder str lst=
		match lst with 
		| [] -> false
		| (var, n)::tl -> (str = var)||(finder str tl)
	in
	let rec cal (env, e) =
		match e with
		| NUM n -> n
		| PLUS (e1, e2) -> (cal (env, e1))+(cal (env, e2))
		| MINUS (e1, e2) -> (cal (env, e1))-(cal (env, e2))
		| MULT (e1, e2) -> (cal (env, e1))*(cal (env, e2))
		| DIVIDE (e1, e2) -> let divisor = (cal (env, e2)) in 
			if divisor = 0 
			then raise (Error "DIVISION_by_zero") 
			else (cal (env, e1))/divisor
		| MAX lst -> 
			(match lst with [] -> 0 | _ -> (List.fold_left findMax min_int lst))
		| VAR var -> if (finder var env) then List.assoc var env else raise (Error "Invalid_variable")
		| LET (var, e1, e2) -> cal ((var, (cal (env, e1)))::env, e2)
	and findMax n exp = max n (cal (env, exp))
	in
	let result = cal (env, e)
	in
	print_int result;result
    
end

    
