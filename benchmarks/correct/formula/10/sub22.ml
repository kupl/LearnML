type formula = 
	True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec eval t =
	let rec inteval x =
		match x with
		Plus(x1, x2) -> (inteval x1) + (inteval x2)
		|Minus(x1, x2) -> (inteval x1) - (inteval x2)
		|Num x -> x
		in
	
	match t with
	Not t -> not (eval t)
	|AndAlso (t1, t2) -> (eval t1) && (eval t2)
	|OrElse (t1, t2) -> (eval t1) || (eval t2)
	|Imply (t1, t2) -> (not (eval t1)) || (eval t2)
	|Equal (t1, t2) -> (inteval t1) = (inteval t2)
	|True -> true
	|False -> false
