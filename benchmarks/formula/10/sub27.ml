exception Error

type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec value exp = match exp with NUM(num) -> num
| PLUS(num1,num2) -> value(num1) + value(num2)
| MINUS(num1, num2) -> value(num1) - value(num2)

let rec eval form = match form with TRUE -> true
| FALSE -> false
| NOT(form1) -> not (eval form1)
| ANDALSO(form1, form2) -> (eval form1) && (eval form2)
| ORELSE(form1, form2) -> (eval form1) || (eval form2)
| IMPLY(form1, form2) -> (not (eval form1)) || (eval form2)
| LESS(exp1, exp2) -> (value exp1) < (value exp2);;