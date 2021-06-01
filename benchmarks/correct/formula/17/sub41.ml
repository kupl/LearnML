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


let rec eval (f : formula) : bool = 
	match f with
	| True -> true
	| False -> false
	| Not x -> not (eval x)
	| AndAlso (x, y) -> (eval x) && (eval y)
	| OrElse (x, y) -> (eval x) || (eval y)
	| Imply (x, y) -> 
		if (false = eval x) then true
		else if (false = eval y && true = eval x) then false
		else true
	| Equal (x, y) -> 
		let rec toInt (e: exp): int = 
			match e with
			Num a -> a
			| Plus (a, b) -> (toInt a) + (toInt b)
			| Minus (a, b) -> (toInt a) - (toInt b)
		in
        let (xInt, yInt) = (toInt x, toInt y) in

        if (xInt = yInt) then true
        else false
