open Printf

module type  ZEXPR =
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
	type environment = id list * value list
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
	type environment = id list * value list
	let emptyEnv = ([], [])
	let rec eval ((en: environment), (ex: expr)) : value =
		match ex with
		| NUM i -> i
		| PLUS (ex1, ex2) -> eval(en, ex1) + eval(en, ex2)
		| MINUS (ex1, ex2) -> eval(en, ex1) - eval(en, ex2)
		| MULT (ex1, ex2) -> eval(en,ex1) * eval(en, ex2)
		| DIVIDE (ex1, ex2) -> eval(en, ex1) / eval(en, ex2)
		| MAX exlist -> 
			let rec  listMax ((l: expr list), (m: value)) : value =
				match l with
				| h::t -> if eval(en, h) > m then listMax(t, eval(en,h)) else listMax(t, m)
				| [] -> m
				in
			(match exlist with
			| h::t -> listMax(t, eval(en, h))
			| [] -> 0 (*raise (Error "empty list")*)
			)
		| VAR ids ->
			let rec checkEnv ((en2: environment), (ids2: id)) : value =
				match en2 with
				| (idh::idt, vh::vt) -> if idh<>ids2 then checkEnv((idt, vt), ids2) else vh 
				| emptyEnv -> raise(Error "FreeVariable")
				in
			checkEnv(en, ids)
		| LET (ids, ex1, ex2) -> eval((ids::(fst en), eval(en, ex1)::(snd en)), ex2) 

	let print_value (v: value) : unit =
		print_newline(print_string(string_of_int(v)))
end
