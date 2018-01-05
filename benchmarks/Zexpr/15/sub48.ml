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
	val print_value : value -> unit
end

module Zexpr : ZEXPR =
struct
	exception Error of string
	type id = string
	type value = int
	type expr =
        | NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr
        | MULT of expr * expr
        | DIVIDE of expr * expr
        | MAX of expr list
        | VAR of id
        | LET of id * expr * expr
	type environment = (id * value) list 
	let emptyEnv = []
        let rec max_list l =
                 match l with
                 | [] -> 0
                 | h::t -> max h (max_list t)

	let rec eval (env1, exp1) =
		match exp1 with
		| NUM i -> i
		| PLUS (i1, i2) ->  eval(env1, i1) + eval(env1, i2)
		| MINUS (i1, i2) ->  eval(env1, i1) - eval(env1, i2)
		| MULT (i1, i2) -> eval(env1, i1) * eval(env1, i2)
		| DIVIDE (i1, i2) -> eval(env1, i1) / eval(env1, i2)
		| VAR id1 -> 
			let rec finding (env1, id1) =
				if List.length env1 = 0 then raise (Error "FreeVariable")
				else if fst (List.hd env1) = id1 then snd (List.hd env1)
				else finding (List.tl env1, id1) 
			in
			finding (env1, id1)
		| LET (id1, ex1, ex2) ->
			let newv = (eval (env1, ex1)) in
			eval ((id1, newv)::env1, ex2)
		| MAX list1 -> 0

	let print_value val1 = print_int val1
	
end

