(*2011-11004 ³²À±¼® ¹®Á¦ 6*)

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
type environment
type value
val emptyEnv: environment
val eval: environment * expr -> value
val int_of_value: value -> int
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
type environment = int
type value = int
let emptyEnv = 0
let rec giVal(vi, i, v) = match v with
	NUM x -> NUM x 
	| PLUS(v1, v2) -> PLUS(giVal(vi, i, v1), giVal(vi, i, v2))
	| MINUS(v1, v2) -> MINUS(giVal(vi, i, v1), giVal(vi, i, v2))
	| MULT(v1, v2) -> MULT(giVal(vi, i, v1), giVal(vi, i, v2))
	| DIVIDE(v1, v2) -> DIVIDE(giVal(vi, i, v1), giVal(vi, i, v2))		
	| MAX(vlist) -> MAX(List.map (fun va -> giVal(vi,i,va)) vlist)
	| VAR id -> i
	| LET(id, v1, v2) -> 
		if vi = id then LET(id, v1, v2)
		else LET(id, v1, giVal(vi, i, v2))
let rec eval(x, y) = match y with
	NUM v -> v
	| PLUS(v1, v2) -> eval(x, v1) + eval(x, v2)
	| MINUS(v1, v2) -> eval(x, v1) - eval(x, v2)
	| MULT(v1, v2) -> eval(x, v1) * eval(x, v2)
	| DIVIDE(v1, v2) -> eval(x, v1) / eval(x, v2)
	| MAX(h::vlist) -> (match vlist with
		[] -> eval(x, h)
		|h2::vplist -> 
			if eval(x, h) > eval(x, h2) then eval(x, MAX(h::vplist))
			else eval(x, MAX(h2::vplist))
		)				 
	| MAX [] -> 0 
	| VAR i -> raise(Error "FreeVariable")
	| LET(i, v1, v2) -> 
		let v2 = giVal(i, v1, v2) in
		eval(x, v2)
let int_of_value x = x
end





