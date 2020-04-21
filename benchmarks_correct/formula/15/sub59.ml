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

let rec calc ex =
	match ex with
	| Num(i) -> i
	| Plus(x,y) -> calc(x) + calc(y)
	| Minus(x,y) -> calc(x) - calc(y)

let rec eval form =
	match form with
	| True -> true
	| False -> false
	| AndAlso(x,y) -> eval(x) && eval(y)
	| Not(x) -> not(eval(x))
	| OrElse(x,y) -> eval(x) || eval(y)
	| Imply(x,y) -> (not(eval(x))) || eval(y)
	| Equal(x,y) -> calc(x) = calc(y)
