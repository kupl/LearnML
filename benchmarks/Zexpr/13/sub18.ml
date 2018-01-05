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

    type environment = ((string * int) list)
    type value = VALUE of int
    
    let emptyEnv = []

    let rec envcheck (env, str) = 
	match env with
	| [] -> false
	| hd::tl -> match hd with
			| (a,b) -> if a = str then true
				else envcheck (tl, str)

    let rec envvar (ev, str) =
	if (envcheck (ev, str))=true then
		match ev with
		| [] -> raise (Error "FreeVariable")
		| hd::tl -> match hd with
				| (a,b) -> if (a=str) then b
					else envvar (tl, str)
	else raise (Error "FreeVariable")


    let rec int_of_value v = 
	match v with
	| VALUE a -> a

    let rec eval (env, e) =
	match (env, e) with
	| (_, NUM a) -> VALUE a
	| (_, PLUS (a,b)) -> VALUE (( int_of_value (eval (env, a))) + (int_of_value (eval (env, b))))
	| (_, MINUS (a,b)) -> VALUE ((int_of_value (eval (env, a))) - (int_of_value (eval (env, b))))
	| (_, MULT (a,b)) -> VALUE ((int_of_value (eval (env,a))) * (int_of_value (eval (env, b))))
	| (_, DIVIDE (a,b)) -> VALUE ((int_of_value (eval (env,a))) / (int_of_value (eval (env, b))))
	| (_, MAX []) -> VALUE 0
	| (_, MAX (hd::[])) -> VALUE (int_of_value (eval (env, hd)))
	| (_, MAX (a::b::tl)) -> if (int_of_value (eval (env, a)))>(int_of_value (eval (env, b)))
				then (eval (env, MAX (a::tl)))
				else (eval (env, MAX (b::tl)))
	| (ev, VAR a) -> VALUE (envvar (ev, a))
	| (ev, LET (id,a,b)) -> eval (((id, (int_of_value (eval (env, a))))::env) , b)


end

