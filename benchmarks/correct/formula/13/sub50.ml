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

let rec cal e =
	match e with
	| Num num -> num
	| Plus (lexp, rexp) -> (cal lexp) + (cal rexp)
	| Minus (lexp, rexp) -> (cal lexp) - (cal rexp)	

let rec eval f =
	match f with
	| True -> true
	| False -> false
	| Not form -> not (eval form)
	| AndAlso (lform, rform) -> (eval lform) && (eval rform)
	| OrElse (lform, rform) -> (eval lform) || (eval rform)
	| Imply (lform, rform) -> (not (eval lform)) || (eval rform)
	| Equal (lexp, rexp) -> (cal lexp) = (cal rexp)
