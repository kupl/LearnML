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
	
	type value = int
	type environment = (id * value) list

	val emptyEnv : environment
	val eval : environment * expr -> value

	val print_value : value -> unit
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

	type value = int
	type environment = (id * value) list
	
	let	emptyEnv = []
	let rec eval (en, ex) =
		match ex with
		| NUM n -> n
		| PLUS (e1, e2) -> eval (en, e1) + eval (en, e2)
		| MINUS (e1, e2) -> eval (en, e1) - eval (en, e2)
		| MULT (e1, e2) -> eval (en, e1) * eval (en, e2)
		| DIVIDE (e1, e2) -> eval (en, e1) * eval (en, e2)
		| MAX a ->(
			match a with
			| [] -> 0
			| h::t ->
				let rec find_max lst m =
					match lst with
					| [] -> m
					| head::tail -> if m > (eval (en, head)) then find_max tail m
									else find_max tail (eval (en, head)) in
				find_max a (eval (en, h)))
		| VAR a -> if (List.mem_assoc a en) then List.assoc a en
					else raise (Error "FreeVariable")
		| LET (i, e1, e2) -> eval((i, eval (en, e1))::en, e2)
	let print_value a = print_endline (string_of_int a)
end


let asdf = Zexpr.LET("x",Zexpr.NUM 1,Zexpr.PLUS(Zexpr.LET("x", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "x")),Zexpr.VAR "x"));;

let _ = Zexpr.print_value (Zexpr.eval ([], asdf));;



