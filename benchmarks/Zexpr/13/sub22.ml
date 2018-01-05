(* * Programming Languages, 2013 Fall. * Skeleton Code for Exercise 2-4 -- *)
(* answer.ml * Joonwon Choi (jwchoi@ropas.snu.ac.kr)                       *)

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
	
	type value = expr
	type environment = (id * value) list
	
	let emptyEnv = []
	let rec eval (env, e) =
		match e with
		| NUM n -> NUM n
		| PLUS (a, b) -> PLUS (eval (env, a), eval (env, b))
		| MINUS (a, b) -> MINUS (eval (env, a), eval (env, b))
		| MULT (a, b) -> MULT (eval (env, a), eval (env, b))
		| DIVIDE (a, b) -> DIVIDE (eval (env, a), eval (env, b))
		| MAX [] -> NUM 0
		| MAX (head::tail) -> MAX ((eval (env, head))::(eval (env, MAX tail))::[])
		| VAR x ->
				if (List.mem_assoc x env) then (List.assoc x env)
				else raise (Error ("FreeVariable"))
		| LET (x, a, b) -> eval ((x, eval (env, a))::env, b)
	
	let rec int_of_value v =
		match v with
		| NUM a -> a
		| PLUS (a, b) -> int_of_value a + int_of_value b
		| MINUS (a, b) -> int_of_value a - int_of_value b
		| MULT (a, b) -> int_of_value a * int_of_value b
		| DIVIDE (a, b) -> int_of_value a / int_of_value b
		| MAX (head::tail) ->
			if List.length tail = 0 then int_of_value head
			else if int_of_value head > int_of_value (MAX tail) then int_of_value head
			else int_of_value (MAX tail)
		| _ -> 0
end