(*
2008-12155
*)

type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec eval pro =
	let rec value exp =
		match exp with
		| Num n -> n
		| Plus(x, y) -> (value x) + (value y)
		| Minus(x, y) -> (value x) - (value y)
	in

	match pro with
	| True -> true
	| False -> false
	| Not form -> (if (eval form) = true then false
		else true)
	| AndAlso(x, y) -> (if (eval x) = false then false
		else if (eval x) = true & (eval y) = true then true
		else false)
	| OrElse(x, y) -> (if (eval x) = true then true
		else if (eval x) = false & (eval y) = false then false
		else true)
	| Imply(x, y) -> (if (eval x) = false then true
		else if (eval x) = true & (eval y) = true then true
		else false)
	| Equal(x, y) -> (if (value x) = (value y) then true
		else false)