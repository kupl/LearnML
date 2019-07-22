type expr = NUM of int
  	| PLUS of expr * expr
  	| MINUS of expr * expr
  
type formula = TRUE | FALSE
  	| NOT of formula
  	| ANDALSO of formula * formula
  	| ORELSE of formula * formula
  	| IMPLY of formula * formula
  	| LESS of expr * expr

let rec etoi exp = 
  	match exp with
  	| NUM a -> a
  	| PLUS (exp1, exp2) -> etoi exp1 + etoi exp2
  	| MINUS (exp1, exp2) -> etoi exp1 - etoi exp2

let rec eval f =
  	match f with
  	| TRUE -> true
  	| FALSE -> false
  	| NOT (p) -> not (eval p)
	| ANDALSO (p, q) -> (eval p) && (eval q)
	| ORELSE (p, q) -> (eval p) || (eval q)
	| IMPLY (p, q) -> not (eval p) || (eval q)
  	| LESS (exp1, exp2) ->
  		if (etoi exp1) < (etoi exp2) then true
		else false
