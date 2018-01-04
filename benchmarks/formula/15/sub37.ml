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

let rec result : expr -> int = fun e ->
 match e with
 | NUM(n) -> n
 | PLUS(n1,n2) -> result(n1)+result(n2)
 | MINUS(n1,n2) -> result(n1)-result(n2)

let rec eval : formula -> bool = fun f ->
 match f with
 | TRUE -> true
 | FALSE -> false
 | NOT(f1) -> not (eval(f1))
 | ANDALSO(f1,f2) -> eval(f1) && eval(f2)
 | ORELSE(f1,f2) -> eval(f1) || eval(f2)
 | IMPLY(f1,f2) -> (not (eval(f1))) && eval(f2)
 | LESS(e1,e2) -> result(e1)<result(e2)