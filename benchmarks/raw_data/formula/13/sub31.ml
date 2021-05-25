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

let rec evalexp ex = 
	match ex with
		Num(i) -> i
		| Plus(e1, e2) -> ((evalexp e1) + (evalexp e2))
		| Minus(e1, e2) -> ((evalexp e1) - (evalexp e2))

let rec eval form = 
	match form with
		True -> true
		| False -> false
		| Not(f) -> (not (eval f))
		| AndAlso(f, g) -> ((eval f) && (eval g))
		| OrElse(f, g) -> ((eval f) || (eval g))
		| Imply(f, g) -> ((not (eval f)) || (eval g))
		| Equal(ex1, ex2) -> ((evalexp ex1) = (evalexp ex2))

