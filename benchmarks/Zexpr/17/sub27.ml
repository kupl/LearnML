(*
 CSE 2012-11226 Kwak Jin Han
 exercise 7
 *)

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

module Zexpr (* : ZEXPR *) =
	struct
		(* implment this module *)
		exception Error of string		(* "FreeVariable" *)
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
		let rec eval (env, expr) = (* environment * expr -> value(int) *)
			match (env, expr) with
			|	(_, NUM i) -> i
			| (_, PLUS (left, right)) -> eval(env, left) + eval(env, right)
			| (_, MINUS (left, right)) -> eval(env, left) - eval(env, right)
			| (_, MULT (left, right)) -> eval(env, left) * eval(env, right)
			| (_, DIVIDE (left, right)) -> eval(env, left) / eval(env, right)
			| (_, MAX l) -> 
					( match l with
						| [] -> 0
						| hd::[] -> eval(env, hd)
						| hd::tl -> 
							if (eval(env, hd) > eval(env, MAX tl)) then eval(env, hd)
							else eval(env, MAX tl) )
			| ([], VAR id) -> raise (Error "FreeVariable")
			| ((str, integer)::tl, VAR id) -> 
					if str = id then integer
					else eval(tl, VAR id)
			| (_, LET(id, left, right)) -> eval((id, eval(env, left))::env, right)

		(* value -> unit *)
		let print_value v = print_int v 
	end

(*
let print = fun x -> Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, x)) 
	let var = fun x -> Zexpr.VAR x 
	let num = fun x -> Zexpr.NUM x 
	let set = fun (x, y, z) -> Zexpr.LET(x, y, z) 
	let plus = fun (x, y) -> Zexpr.PLUS(x, y) 
	let minus = fun (x, y) -> Zexpr.MINUS(x, y) 
	let div = fun (x, y) -> Zexpr.DIVIDE(x, y) 
let mul = fun (x, y) -> Zexpr.MULT(x, y) 
	let max = fun x -> Zexpr.MAX x 

	let _ = print(num 1), print_string "Case 1 : 1 vs " 
	let _ = print(set("x", num 1, plus(set("x", num 2, plus(var "x", var "x")), var "x"))), print_string "Case 2 : 5 vs " 
	let _ = print(max []), print_string "Case 3 : 0 vs " 
	let _ = print(max [num(-1); num(-2); num(-3)]), print_string "Case 4 : -1 vs " 
	let _ = print(div(num 3, num 2)), print_string "Case 5 : 1 vs " 
	let _ = print(plus(num 7, num 9)), print_string "Case 6 : 16 vs " 
	let _ = print(minus(num 7, num 9)), print_string "Case 7 : -2 vs " 
	let _ = print(mul(num 7, num 9)), print_string "Case 8 : 63 vs " 
	let _ = print(set("x", num 1, plus(set("y", num 2, plus(var "x", var "y")), var "x"))), print_string "Case 9 : 4 vs " 
	let _ = print(set("x", num 1, set("y", num 2, set("z", num(-1), max[var "x"; var "y"; var "z"])))), print_string "Case 10 : 2 vs " 
	let _ = try print(set("x", num 1, set("y", num 2, set("z", num(-1), max[var "x"; var "y"; var "z"; var "a"])))) with Zexpr.Error x -> 
	if (x = "FreeVariable") then print_endline("Error Case 1 : Pass") 
	else print_endline("Error Case 1 : Failure") 
	let _ = try print(set("x", num 1, plus(set("y", num 2, plus(var "x", var "y")), var "y"))) with Zexpr.Error x -> 
	if (x = "FreeVariable") then print_endline("Error Case 2 : Pass") 
	else print_endline("Error Case 2 : Failure")
	*)
