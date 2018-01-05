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
		val print_value : value -> unit
	end


module Zexpr =
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
		type environment = (string * int) list
		type value = int
		let emptyEnv = []

		let rec findEnv env str =
			match env with
			| [] -> 0
			| h::t -> 
				match h with
				| (a,b) -> if (compare a str) == 0 then b
					else findEnv t str
		let rec eval = function
			| (env, expr) ->
				match expr with
				| NUM(a) -> a
				| PLUS (a,b) -> eval(env, a) + eval(env, b)
				| MINUS (a,b) -> eval(env, a) - eval(env, b)
				| MULT (a,b) -> eval(env, a) * eval(env, b)
				| DIVIDE (a,b) -> eval(env, a) / eval(env, b)
				| MAX ([]) -> 0
				| MAX (h :: t) -> (
						let rec calculmax max one =
							match max with
							| [] -> one
							| h :: t -> if eval(env, h) > one then calculmax t (eval(env, h))
								else calculmax t one in
						calculmax t (eval(env, h))) 
				| LET (a, b, c) ->
					eval ((a, eval(env, b)) :: env, c)
				| VAR (a) -> findEnv env a
		let print_value a = print_int a
	end					


let print = fun x -> Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, x)) 
let var = fun x -> Zexpr.VAR x 
let num = fun x -> Zexpr.NUM x 
let set = fun (x, y, z) -> Zexpr.LET(x, y, z) 
let plus = fun (x, y) -> Zexpr.PLUS(x, y) 
let minus = fun (x, y) -> Zexpr.MINUS(x, y) 
let div = fun (x, y) -> Zexpr.DIVIDE(x, y) 
let mul = fun (x, y) -> Zexpr.MULT(x, y) 
let max = fun x -> Zexpr.MAX x 

let _ = print(mul(num 7, num 9)), print_string "Case 8 : 63 vs " 
let _ = print(set("x", num 1, plus(set("y", num 2, plus(var "x", var "y")), var "x"))), print_string "Case 9 : 4 vs " 
let _ = print_string "\n"
let _ = Zexpr.print_value 3
let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.NUM 1))
let _ = print(set("x", num 1, set("y", num 2, set("z", num(-1), max[var "x"; var "y"; var "z"])))), print_string "Case 10 : 2 vs   "
