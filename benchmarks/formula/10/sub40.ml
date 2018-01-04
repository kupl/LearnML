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

let rec eval form = 
	let rec count x = match x with
		NUM n -> n
		|PLUS (a,b) -> (count a) + (count b)
		|MINUS (a,b) -> (count a) - (count b)
	in
	match form with
	TRUE -> true
	|FALSE -> false
	|NOT f -> not (eval f)
	|ANDALSO (a,b) -> (eval a) && (eval b)
	|ORELSE (a,b) -> (eval a) || (eval b)
	|IMPLY (a,b) -> if ((eval a) && (not (eval b))) then false else true
	|LESS (c,d) -> (count c) < (count d)
;;
