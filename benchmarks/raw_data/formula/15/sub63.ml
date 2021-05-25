type exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp
	
type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp

let rec calc (e: exp): int =
	match e with
		| Num n -> n
		| Plus(n1, n2) -> (calc n1) + (calc n2)
		| Minus(n1, n2) -> (calc n1) - (calc n2)	
	
let rec eval (f: formula): bool =
  match f with
		| True -> true
		| False -> false
		| Not sf ->
			if eval sf then false else true
		| AndAlso(sf1, sf2) ->
			if not (eval sf1) then false
			else if not (eval sf2) then false 
			else true
		| OrElse(sf1, sf2) ->
			if eval sf1 then true
			else if eval sf2 then true
			else false
		| Imply(sf1, sf2) -> 
			if (eval sf1 && not (eval sf2)) then false else true
		| Equal(ex1, ex2) -> if (calc ex1)=(calc ex2) then true else false