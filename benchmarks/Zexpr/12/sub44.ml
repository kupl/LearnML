module type ZEXPR = sig
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
	type environment = (id * int) list
	type value = int
	val emptyEnv: environment
	val eval: environment * expr -> value
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
        type environment = (id * int) list
        type value = int
        let emptyEnv = []
	let eval (a, b) =
		let rec evalfun (a, b) = 
			let rec maxfunc (l, max) =
				match l with
				| [] -> max
				| hd::tl -> if (evalfun (a, hd)) > max then (maxfunc (tl, (evalfun (a, hd))))
					else (maxfunc (tl, max))
			in
				
			
			match b with
			| NUM n -> n
			| PLUS (e1, e2) -> (evalfun (a, e1)) + (evalfun (a, e2))
			| MINUS (e1, e2) -> (evalfun (a, e1)) + (evalfun (a, e2))
			| MULT (e1, e2) -> (evalfun (a, e1)) * (evalfun (a, e2))
			| DIVIDE (e1, e2) -> (evalfun (a, e1)) / (evalfun (a, e2))
			| MAX [] -> 0
			| MAX l -> (maxfunc ((List.tl l), (evalfun (a, (List.hd l)))))
			| VAR x -> if (List.mem_assoc x a) then (List.assoc x a)
				else raise (Error "No val in Environment")
			| LET (x, e1, e2) -> (evalfun (((x, (evalfun (a, e1)))::a), e2))
		in
		
		((print_endline (string_of_int (evalfun (a, b)))); (evalfun (a, b)))
		
end
