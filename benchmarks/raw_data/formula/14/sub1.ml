(* 2006-11377 hw1-2 *)

type exp = 
  Num of int
| Plus of exp * exp
| Minus of exp * exp

type formula = 
  True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp

let rec eval form = 
	match form with
	| True -> true
	| False -> false
	| Not f -> not (eval f)
	| AndAlso (f1, f2) -> (eval f1) && (eval f2)
	| OrElse (f1, f2) -> (eval f1) || (eval f2)
	| Imply (f1, f2) -> (not (eval f1)) || (eval f2)
	| Equal (e1, e2) -> 
		let rec cal exp = 
			match exp with
			| Num n -> n
			| Plus (e1, e2) -> (cal e1) + (cal e2)
			| Minus (e1, e2) -> (cal e1) - (cal e2)
		in
		if (cal e1) = (cal e2) then true
		else false