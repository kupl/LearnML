type exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp;;

type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp;;

let rec evalexp e =
	match e with
	|Num t -> t
	|Plus(e1,e2) -> evalexp(e1) + evalexp(e2)
	|Minus(e1,e2) -> evalexp(e1) - evalexp(e2);;

let rec eval f =
	match f with
	|True -> true
	|False -> false
	|Not tail -> if eval(tail)==true then false else true
	|AndAlso(t1, t2) -> if eval(t1) && eval(t2) then true else false
	|OrElse(t1, t2) -> if eval(t1) || eval(t2) then true else false
	|Imply(t1, t2) -> if eval(t1) && (not(eval(t2))) then false else true
	|Equal(e1, e2) -> evalexp(e1) = evalexp(e2);;