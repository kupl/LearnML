exception Error

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

let rec value exp = match exp with Num(num) -> num
| Plus(num1,num2) -> value(num1) + value(num2)
| Minus(num1, num2) -> value(num1) - value(num2)

let rec eval form = match form with True -> true
| False -> false
| Not(form1) -> not (eval form1)
| AndAlso(form1, form2) -> (eval form1) && (eval form2)
| OrElse(form1, form2) -> (eval form1) || (eval form2)
| Imply(form1, form2) -> (not (eval form1)) || (eval form2)
| Equal(exp1, exp2) -> (value exp1) = (value exp2)