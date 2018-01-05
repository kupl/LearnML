module type ZEXPR = 
sig    
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

    type environment = id*expr
    type value = expr

    let emptyEnv = ("", NUM(0))
    let rec eval (env, e) = match e with
	| NUM(num)-> NUM(num)
	| PLUS(a, b) -> PLUS((eval (env, a)), (eval (env, b)))
	| MINUS(a, b) -> MINUS((eval (env, a)), (eval (env, b)))
	| MULT(a, b) -> MULT((eval (env, a)), (eval (env, b)))
	| DIVIDE(a, b) -> DIVIDE((eval (env, a)), (eval (env, b)))
	| MAX(li) -> if ((List.length li)=0) then NUM(0) else MAX((List.map (fun b-> eval(env, b)) li))
	| LET(name, sameTo, at) -> eval (env, (eval ((name, sameTo), at)))
	| VAR(id) -> match env with 
				(st, exp)-> if id=st then exp else VAR(id)
	
 

    let rec int_of_value v = match v with
	| NUM(num) -> num
	| PLUS(a, b) -> int_of_value a + int_of_value b
	| MINUS(a, b) -> int_of_value a - int_of_value b
	| MULT(a, b) -> int_of_value a * int_of_value b
	| DIVIDE(a, b) -> int_of_value a / int_of_value b
	| MAX(li) -> List.hd  (List.sort (fun a b -> b-a) (List.map (fun a-> int_of_value a) li))
	| VAR(_) -> raise (Error "FreeVariable")
	| LET(_,_,_) -> raise (Error "FreeVariable")
end
