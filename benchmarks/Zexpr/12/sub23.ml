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
		let rec myeval (env, e) =
			match e with
				| NUM n -> n
				| PLUS (e1, e2) -> (myeval (env, e1)) + (myeval (env, e2))
				| MINUS (e1, e2) -> (myeval (env, e1)) - (myeval (env, e2))
				| MULT (e1, e2) -> (myeval (env, e1)) * (myeval (env, e2))
				| DIVIDE (e1, e2) -> (myeval (env, e1)) / (myeval (env, e2))
				| MAX lst -> (match lst with
					| [] -> 0
					| _ -> (List.fold_left mymax min_int lst))
				| VAR x -> List.assoc x env
				| LET (var, e1, e2) -> myeval ((var, (myeval (env, e1)))::env, e2)
		and mymax m ex = max m (myeval (env, ex))
		in
	let ans = myeval (env, e) in
	print_int ans;
	ans
    
end