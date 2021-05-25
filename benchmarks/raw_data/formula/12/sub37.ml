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


let rec eval fm =
let rec cal exp =
match exp with
|Num i -> i
|Plus (a,b) -> (cal a) + (cal b)
|Minus (a,b) -> (cal a) - (cal b)
in
match fm with
|True -> true
|False -> false
|Not x -> if (eval x) = true then false else true
|AndAlso (x, y) -> if (eval x) = true && (eval y) = true then true
			else false
|OrElse (x, y) -> if (eval x) = true || (eval y) = true then true
			else false
|Imply (x, y) -> if ((eval x) = true && (eval y) = true) || (eval x) = false
		then true
		else false
|Equal (exp1, exp2) -> if (cal exp1) = (cal exp2) then true
			else false

