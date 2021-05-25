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

let rec eval_exp x =
	match x with
	| (Num n) -> n
	| (Plus (a, b)) -> ((eval_exp a) + (eval_exp b))
	| (Minus (a, b)) -> ((eval_exp a) - (eval_exp b))

let rec eval x =
	match x with
	| (Not f) -> (not (eval f))
	| (AndAlso (f1, f2)) -> ((eval f1) && (eval f2))
	| (OrElse (f1, f2)) -> ((eval f1) || (eval f2))
	| (Imply (f1, f2)) -> ((not (eval f1)) || (eval f2))
	| (Equal (f1, f2)) -> ((eval_exp f1) = (eval_exp f2))
	| True -> true
	| False -> false
