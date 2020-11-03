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

let rec evalexp e =
	match e with
	| Num n -> n
	| Plus (e0, e1) -> (evalexp e0) + (evalexp e1)
	| Minus (e0, e1) -> (evalexp e0) - (evalexp e1)

let rec eval f =
	match f with
	| True -> true
	| False -> false
	| Not f0 -> not (eval f0)
	| AndAlso (f0, f1) -> (eval f0) && (eval f1)
	| OrElse (f0, f1) -> (eval f0) || (eval f1)
	| Imply (f0, f1) -> not (eval f0) || ((eval f0) && (eval f1))
	| Equal (e0, e1) -> (evalexp e0) = (evalexp e1)

