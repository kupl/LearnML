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

let rec exVal x =
	match x with
		| NUM n -> n
		| PLUS (a, b) -> (exVal a) + (exVal b)
		| MINUS (c, d) -> (exVal c) - (exVal d)

let rec eval f = 
	match f with
		| TRUE -> true
		| FALSE -> false
		| NOT n -> if (eval n) = true then false
					else true
		| ANDALSO (a, b) -> if ((eval a) = true) && ((eval b) = true) then true
						else false
		| ORELSE (c, d) -> if ((eval c) = false) && ((eval d) = false) then false
						else true
		| IMPLY (e, f) -> if ((eval e) = true) && ((eval f) = false) then false
						else true
		| LESS (g, h) -> if ((exVal g) < (exVal h)) then true
						else false