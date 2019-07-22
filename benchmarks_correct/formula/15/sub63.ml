type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	
type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr

let rec calc (e: expr): int =
	match e with
		| NUM n -> n
		| PLUS(n1, n2) -> (calc n1) + (calc n2)
		| MINUS(n1, n2) -> (calc n1) - (calc n2)	
	
let rec eval (f: formula): bool =
  match f with
		| TRUE -> true
		| FALSE -> false
		| NOT sf ->
			if eval sf then false else true
		| ANDALSO(sf1, sf2) ->
			if not (eval sf1) then false
			else if not (eval sf2) then false 
			else true
		| ORELSE(sf1, sf2) ->
			if eval sf1 then true
			else if eval sf2 then true
			else false
		| IMPLY(sf1, sf2) -> 
			if (eval sf1 && not (eval sf2)) then false else true
		| LESS(ex1, ex2) -> if (calc ex1)<(calc ex2) then true else false