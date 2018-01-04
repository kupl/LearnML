type formula = TRUE 
			| FALSE
			| NOT of formula
			| ANDALSO of formula * formula
			| ORELSE of formula * formula
			| IMPLY of formula * formula
			| LESS of expr * expr
and expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr


let rec eval (f : formula) : bool = 
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT x -> not (eval x)
	| ANDALSO (x, y) -> (eval x) && (eval y)
	| ORELSE (x, y) -> (eval x) || (eval y)
	| IMPLY (x, y) -> 
		if (false = eval x) then true
		else if (false = eval y && true = eval x) then false
		else true
	| LESS (x, y) -> 
		let rec toInt (e: expr): int = 
			match e with
			NUM a -> a
			| PLUS (a, b) -> (toInt a) + (toInt b)
			| MINUS (a, b) -> (toInt a) - (toInt b)
		in
        let (xInt, yInt) = (toInt x, toInt y) in

        if (xInt < yInt) then true
        else false
