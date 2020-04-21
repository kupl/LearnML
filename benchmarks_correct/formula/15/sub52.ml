type exp = Num of int
| Plus of exp * exp
| Minus of exp * exp;;

type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp;;

let rec e_eval = fun x->
	match x with
	|Num(i) -> i
	|Plus(i1,i2) -> e_eval(i1) + e_eval(i2)
	|Minus(i1,i2) -> e_eval(i1) - e_eval(i2)

let rec eval = fun x-> 
	match x with 
	|True -> true
	|False -> false
	|Not(f) -> not(eval(f))
	|AndAlso(f1,f2) -> eval(f1) && eval(f2)
	|OrElse(f1,f2) -> eval(f1) || eval(f2)
	|Imply(f1,f2) -> not(eval(f1))||eval(f2)
	|Equal(e1,e2) -> e_eval(e1) = e_eval(e2)

