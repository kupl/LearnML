(*2006 11720 2-3 KimEunSol*)
type expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr
type formula = TRUE | FALSE | NOT of formula | ANDALSO of formula * formula
				|ORELSE	of formula*formula | IMPLY of formula * formula
				|LESS of expr*expr

let rec eval_exp(e) = match e with
	NUM a -> a
	|PLUS(a,b) -> eval_exp(a) + eval_exp(b)
	|MINUS(a,b) -> eval_exp(a) - eval_exp(b)
let rec eval(a) = match a with 
	TRUE -> true
	|FALSE -> false
	|NOT(b) -> (if eval(b) = true then false else true)
	|ANDALSO(b,c) -> eval(b)&&eval(c)
	|ORELSE(b,c) -> eval(b)||eval(c)
	|IMPLY(b,c) -> (if eval(b) = true then true 
					else if eval(c) = false then true
					else false)
	|LESS(b, c) -> (if eval_exp(b) < eval_exp(b) then true else false)


