type expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr
type formula = TRUE | FALSE | NOT of formula | ANDALSO of formula * formula | ORELSE of formula * formula | IMPLY of formula * formula | LESS of expr * expr

let rec eval form = 
	let rec getValue exp = 
		match exp with
		NUM n -> n
		|PLUS  (a, b) -> (getValue a) + (getValue b)
		|MINUS (a, b) -> (getValue a) - (getValue b)
	in
	match form with
	TRUE -> true
	|FALSE -> false
	|NOT a -> if (eval a) then false 
		  else true
	|ANDALSO (a, b) -> (eval a) && (eval b)
	|ORELSE (a,b) -> (eval a) || (eval b)
	|IMPLY (a,b) ->
		if ((eval a) && (eval (NOT b))) then false
		else true
	|LESS (a,b) ->
		if ((getValue a) < (getValue b)) then true
		else false
	|_ -> true
	
		
