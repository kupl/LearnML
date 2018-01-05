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

module Zexpr : ZEXPR = 
struct    
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

    type environment = ENV of id * expr
    type value = expr
    
    let emptyEnv = ENV("", NUM 0)

    let rec eval (env, e) = 
	match (env, e) with
	| (ENV("", a), e) -> e
	| (ENV(x, a), NUM i) -> (NUM i)
	| (ENV(x, a), VAR y) -> if (compare x y = 0) then a else VAR y
	| (ENV(x, a), PLUS(p, q)) -> (PLUS(eval(ENV(x, a), p), eval(ENV(x, a), q)))
	| (ENV(x, a), MINUS(p, q)) -> (MINUS(eval(ENV(x, a), p), eval(ENV(x, a), q)))
	| (ENV(x, a), MULT(p, q)) -> (MULT(eval(ENV(x, a), p), eval(ENV(x, a), q)))
	| (ENV(x, a), DIVIDE(p, q)) -> (DIVIDE(eval(ENV(x, a), p), eval(ENV(x, a), q)))
	| (ENV(x, a), MAX []) -> (NUM 0)
	| (ENV(x, a), MAX(s::l)) -> (MAX[eval(ENV(x, a), s); eval(ENV(x, a), MAX l)])
	| (ENV(x, a), LET (y, p, q)) -> eval(ENV(x, a), eval(ENV(y, p), q))

    let rec int_of_value v = 
	match v with
	| (NUM a) -> a
	| (VAR x) -> raise (Error "FreeVariable")
	| (PLUS (p, q)) -> int_of_value(p) + int_of_value(q)
	| (MINUS (p, q)) -> int_of_value(p) - int_of_value(q)
        | (MULT (p, q)) -> int_of_value(p) * int_of_value(q)
        | (DIVIDE (p, q)) -> int_of_value(p) / int_of_value(q)
	| (MAX []) -> 0
	| (MAX [a; b]) -> if (int_of_value(a) > int_of_value(b)) then int_of_value(a)
			  else int_of_value(b)
	| (MAX(a::l)) -> if (int_of_value(a) > int_of_value(MAX l)) then int_of_value(a)
			else int_of_value(MAX l)
	| (LET(a, b, c)) -> int_of_value(eval(ENV(a, b), c))

end
