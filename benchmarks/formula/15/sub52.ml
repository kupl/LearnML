type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr;;

type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr;;

let rec e_eval = fun x->
	match x with
	|NUM(i) -> i
	|PLUS(i1,i2) -> e_eval(i1) + e_eval(i2)
	|MINUS(i1,i2) -> e_eval(i1) - e_eval(i2)

let rec eval = fun x-> 
	match x with 
	|TRUE -> true
	|FALSE -> false
	|NOT(f) -> not(eval(f))
	|ANDALSO(f1,f2) -> eval(f1) && eval(f2)
	|ORELSE(f1,f2) -> eval(f1) || eval(f2)
	|IMPLY(f1,f2) -> not(eval(f1))||eval(f2)
	|LESS(e1,e2) -> e_eval(e1) < e_eval(e2)

