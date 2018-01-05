(* 2006-11867 Jo, Dong-Chul *)
type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

exception DivideByZero

let rec eval : expr -> int = fun e ->
	match e with
	| NUM i -> i
	| PLUS (le, re) -> (eval le) + (eval re)
	| MINUS (le, re) -> (eval le) - (eval re)
	| MULT (le, re) -> (eval le) * (eval re)
	| DIVIDE (le, re) ->
		if (eval re) = 0 then raise DivideByZero
		else (eval le) / (eval re)
	| MAX [] -> 0
	| MAX (fe::[]) -> eval fe
	| MAX (fe::rl) ->
		if (eval fe) < (eval (MAX rl)) then eval(MAX rl)
		else eval fe
