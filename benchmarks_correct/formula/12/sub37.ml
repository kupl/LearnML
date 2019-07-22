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


let rec eval fm =
let rec cal exp =
match exp with
|NUM i -> i
|PLUS (a,b) -> (cal a) + (cal b)
|MINUS (a,b) -> (cal a) - (cal b)
in
match fm with
|TRUE -> true
|FALSE -> false
|NOT x -> if (eval x) = true then false else true
|ANDALSO (x, y) -> if (eval x) = true && (eval y) = true then true
			else false
|ORELSE (x, y) -> if (eval x) = true || (eval y) = true then true
			else false
|IMPLY (x, y) -> if ((eval x) = true && (eval y) = true) || (eval x) = false
		then true
		else false
|LESS (exp1, exp2) -> if (cal exp1) < (cal exp2) then true
			else false

