
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
	val print_value : value -> unit
end

module Vars = Map.Make(String)

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
	type environment = int Vars.t
	type value = int
	let emptyEnv = Vars.empty
	let rec eval (e, exp) = match exp with
		| NUM x -> x
		| PLUS (x, y) -> ( eval(e, x) + eval(e, y) )
		| MINUS (x, y) -> ( eval(e, x) - eval(e, y) )
		| MULT (x, y) -> ( eval(e, x) * eval(e, y) )
		| DIVIDE (x, y) -> ( eval(e, x) / eval(e, y) )
		| MAX [] -> 0
		| MAX [x] -> eval(e, x)
		| MAX (x::lst) -> (max (eval(e, x)) (eval(e, MAX lst)))
		| VAR id -> (if (Vars.mem id e) then (Vars.find id e)
					else ( raise(Error "FreeVariable")) )
		| LET (id, v, x) -> eval(Vars.add id (eval (e, v)) e, x)

	let print_value x = print_int x

end 
