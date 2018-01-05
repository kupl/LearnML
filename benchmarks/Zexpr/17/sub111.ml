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
	type environment = string list
	type value = int
	let emptyEnv : environment = []
	
	let rec searchEnv ((id1 : id), (en : environment)) : int =
		match en with
					 [] | _::[] -> raise (Error "FreeVariable")
					|hd::(hd2::tl)-> if(hd=id1) then (int_of_string hd2) else searchEnv(id1,tl) 
	
	let rec findMax ((l : expr list), (en : environment)) : int =
		match l with
					[] -> 0
					|hd::[] -> eval (en,hd)
					|hd::(hd2::tl) -> if(eval(en,MAX([hd2]@tl)) < eval(en,hd)) then eval(en,hd) else eval (en,MAX([hd2]@tl))


	and eval ((en : environment), (ex : expr)) : value =
		match ex with
					 NUM (int1) -> int1
					|PLUS (expr1,expr2) -> eval (en,expr1) + eval (en,expr2)
					|MINUS (expr1,expr2) -> eval (en,expr1) - eval (en,expr2)
					|MULT (expr1,expr2) -> eval (en,expr1) * eval (en,expr2)
					|DIVIDE (expr1,expr2) -> eval (en,expr1) / eval (en,expr2)
					|MAX (exprlist) -> findMax(exprlist,en)
					|VAR (id1) -> searchEnv(id1, en)
					|LET (id1, expr1, expr2) -> eval (([id1]@[string_of_int (eval (en,expr1))]@en),expr2)

	
	let print_value (v : value) : unit =
		print_int v
end

