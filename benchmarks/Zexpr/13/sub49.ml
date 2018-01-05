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
    type value = | BASE | V of id * int
    type environment = value list
    let emptyEnv = []

    let int_of_value v = match v with | V(value, digit) -> digit | BASE -> -999999999

    let name_of_value v = match v with | V(value, digit) -> value | BASE -> ""

    let iov v = int_of_value v

    let nov v = name_of_value v

    let findMax a b =
        if(iov(a) > iov(b)) then a else b

    let rec eval(env, e) = match e with
	| NUM(x) -> V("", x)
	| PLUS(x, y) -> V("", iov(eval(env, x)) + iov(eval(env, y)))
	| MINUS(x, y) -> V("", iov(eval(env, x)) - iov(eval(env, y)))
	| MULT(x, y) -> V("", iov(eval(env, x)) * iov(eval(env, y)))
	| DIVIDE(x, y) -> V("", iov(eval(env, x)) / iov(eval(env, y)))
	| MAX(xlist) -> let newList = (List.map (fun epr -> eval(env, epr)) xlist) in (List.fold_left findMax BASE newList)
	| VAR(x) -> (
		try
			let elem = List.find (fun v -> (nov(v) = x)) env in elem
		with Not_found -> raise (Error "FreeVariable")
	)
	| LET(x, xVal, e) -> eval(V(x, iov(eval(env, xVal)))::env, e)

end

