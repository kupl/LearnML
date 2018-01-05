module type ZEXPR = sig
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
	type environment = (id * int) list
	type value = int
	
	val emptyEnv: environment
	val eval: environment * expr -> value
	val print_value : value -> unit
end

module Zexpr : ZEXPR = struct
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
	type environment = (id * int) list
	type value = int
	
	let emptyEnv = []
	let rec eval (v, e) =
		match e with
		NUM(nint)	-> nint
		|PLUS(pexpa, pexpb)	-> eval (v, pexpa) + eval (v, pexpb)
		|MINUS(mexpa, mexpb)	-> eval (v, mexpa) - eval (v, mexpb)
		|MULT(mlexpa, mlexpb)	-> eval (v, mlexpa) * eval (v, mlexpb)
		|DIVIDE(dexpa, dexpb)	-> eval (v, dexpa) / eval (v, dexpb)
		|MAX(melist)	-> if melist = [] then 0
							else if List.tl melist = [] then eval (v, List.hd melist)
							else if (eval (v, (List.hd melist))) >= (eval (v, (List.nth melist 1))) then eval (v, MAX((List.hd melist)::(List.tl (List.tl melist))))
							else eval (v, MAX(List.tl melist))
		|VAR(vid)	-> if List.find_all (fun p -> if fst p = vid then true else false) v = []
						then raise (Error "FreeVariable")
						else snd (List.find (fun p -> if fst p = vid then true else false) v)
		|LET(lid, lexpa, lexpb)	-> eval ((lid, (eval (v, lexpa)))::v, lexpb)
	let  print_value v = print_int v; print_endline ""
end
