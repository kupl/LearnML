module type  ZEXPR = 
sig
	exception Error of string
	type id = string
	type expr =
		|NUM of int
		|PLUS of expr * expr
		|MINUS of expr * expr
		|MULT of expr * expr
		|DIVIDE of expr * expr
		|MAX of expr list
		|VAR of id
		|LET of id * expr * expr
	
	type environment
	type value

	val emptyEnv: environment
	val eval: environment * expr -> value

	val print_value : value -> unit
end

module Zexpr : ZEXPR = 
struct
	exception Error of string
	type id = string
	type expr = 
		|NUM of int
		|PLUS of expr * expr
		|MINUS of expr * expr
		|MULT of expr * expr
		|DIVIDE of expr * expr
		|MAX of expr list
		|VAR of id
		|LET of id * expr * expr

	type environment = (string * int) list
	type value = int

	let emptyEnv: environment = []
	let rec eval = fun (e, ex) -> 
		match ex with
			|NUM i -> i
			|PLUS (a, b) -> eval (e, a) + eval (e, b)
			|MINUS (a, b) -> eval (e, a) - eval (e, b)
			|MULT (a, b) -> eval (e, a) * eval (e, b)
			|DIVIDE (a, b) -> eval (e, a) / eval (e, b)
			|MAX el ->
					let rec maxhelper = fun (lst, m) ->
					match lst with
					|[] -> eval (e, m)
					|hd::tl -> if (eval (e, hd) > eval (e, m)) then maxhelper(tl, hd)
						else maxhelper(tl, m)
					in 
					(match el with
					|[] -> 0
					|hd::tl  -> maxhelper (el, hd))
			|VAR v -> 
				let rec find = fun (en, i) ->
					match en with
					|[] -> raise (Error "FreeVariable")
					|hd::tl ->
						(if(fst hd = i) then snd hd
						else find(tl, i))
					in find (e, v)
			|LET (i, a, b) -> eval(((i, eval(e, a))::e), b) 
	let print_value  = fun v -> print_string (string_of_int v)
end
