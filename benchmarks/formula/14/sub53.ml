type formula = TRUE
	|FALSE
	|NOT of formula
	|ANDALSO of formula * formula
	|ORELSE of formula * formula
	|IMPLY of formula * formula
	|LESS of expr * expr
and expr = NUM of int
	|PLUS of expr * expr
	|MINUS of expr * expr
	;;
let rec calc ex = 
	match ex with
	|NUM n  -> n
	|PLUS (a,b) -> (calc a) + (calc b)
	|MINUS (a,b) -> (calc a) - (calc b)
	;;
let rec eval f = 
	match f with
	|TRUE -> true
	|FALSE -> false
	|NOT a -> if(eval a = true) then false
		else true
	|ANDALSO (a,b) -> if (eval a) = false then false
			else if (eval b) = false then false
			else true
	|ORELSE (a,b) -> if(eval a) = true then true
			else if (eval b) = true then true
			else false
	|IMPLY (a,b) -> if(eval a) = true then (eval b)
			else true
	|LESS (a,b) -> if (calc a) < (calc b) then true
			else false
	;;

