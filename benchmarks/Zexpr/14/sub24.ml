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
	val emptyEnv: environment
	val eval: environment * expr -> value

	val int_of_value : value -> int
end

module Zexpr: ZEXPR =
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
	type value = int
	type environment = (id * value) list
	(*stack처럼 사용. 앞으로 넣고, 앞에서부터 읽기*)

	let emptyEnv = [] 

	let rec findv env vname = 
		(* environment 안에서 vname을 id로 가진 expr을 ret *)
		match env with
		| [] -> raise (Error "FreeVariable")
		| (v, a)::remain -> if(v = vname) then a
						else (findv remain vname)
			(* str cmp 방법이 뭐더라.. *)

	let rec eval (env, ex) =
		let max a b =
			if a > b then a else b
		in

		match ex with
		| NUM i -> i
		| PLUS (a, b) -> (eval (env, a)) + (eval (env, b))
		| MINUS (a, b) -> (eval (env, a)) - (eval (env, b))
		| MULT (a, b) -> (eval (env, a)) * (eval (env, b))
		| DIVIDE (a, b) -> (eval (env, a)) / (eval (env, b))
		| MAX t ->(
			match t with 
			| [] -> 0
			| fst::[] -> (eval (env, fst))
			| fst::remain -> (max (eval (env, fst)) (eval (env, (MAX remain))))
		)
		| VAR vname -> (findv env vname)
		| LET (vname, vvalue, b) -> (eval ((vname, (eval (env, vvalue)))::env, b))
			





	let int_of_value valu = valu



end	

(* module AAA = (Zexpr: ZEXPR) *)
