(* complete *)
type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list
;;

exception DividedByZero

let rec eval exp = 
	let f a b = if a < b then 1 else -1	in
	match exp with
	NUM n -> n
	| PLUS (a,b) -> (eval a) + (eval b)
	| MINUS (a,b) -> (eval a) - (eval b)
	| MULT (a,b) -> (eval a) * (eval b)
	| DIVIDE (a,b) -> if (eval b) = 0 then raise DividedByZero else (eval a) / (eval b)
	| MAX lst -> (match (List.sort f (List.map eval lst)) with
			h::t -> h
			|[] -> 0
			)
;;
