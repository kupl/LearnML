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

let rec eval form =
	let rec etoint e =
		match e with
		| Num a -> a
		| Plus (e1, e2) -> (etoint e1) + (etoint e2)
		| Minus (e1, e2) -> (etoint e1) - (etoint e2)
	in

	match form with
	| True -> true
	| False -> false
	| Not form -> (not (eval form))
	| AndAlso (f1, f2) -> (eval f1) && (eval f2)
	| OrElse (f1, f2) -> (eval f1) || (eval f2)
	| Imply  (f1, f2) -> (not (eval f1)) || (eval f2)
	| Equal (e1, e2) -> ((etoint e1) = (etoint e2))
