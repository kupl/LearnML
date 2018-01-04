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

let rec calc = fun expr ->
	match expr with
	|NUM i -> i
	|PLUS (lexpr, rexpr) -> (calc lexpr) + (calc rexpr)
	|MINUS (lexpr, rexpr) -> (calc lexpr) - (calc rexpr)
let rec eval = fun formula ->
	match formula with
	|TRUE -> true
	|FALSE -> false
	|NOT form -> 
		(match (eval form) with
		|true -> false
		|false -> true)
	|ANDALSO (lform, rform) ->
		(match (eval lform) with
		|false -> false
		|true -> (eval rform))
	|ORELSE (lform, rform) ->
		(match (eval lform) with
		|true-> true
		|false -> (eval rform))
	|IMPLY (lform, rform) ->
		(match (eval lform) with
		|false -> true
		|true -> (eval rform))
	|LESS (lex, rex) -> 
		if ((calc lex) < (calc rex)) then true
		else false
