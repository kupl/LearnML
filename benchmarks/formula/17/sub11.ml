type formula = TRUE | FALSE | NOT of formula
| ANDALSO of formula * formula | ORELSE of formula * formula
| IMPLY of formula * formula | LESS of expr * expr
and expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr

let rec eval : formula -> bool = fun x -> match x with
| TRUE -> true
| FALSE -> false
| NOT(f) -> not(eval f)
| ANDALSO(a, b) -> ( eval a && eval b )
| ORELSE(a, b) -> ( eval a || eval b )
| IMPLY(a, b) -> ( not (eval a) || (eval b) )
| LESS(a, b) -> if ( ( evalExpr a ) < ( evalExpr b ) ) then true else false
and evalExpr ( x : expr ) = match x with
| NUM(i) -> i
| PLUS(a, b) -> ( evalExpr a ) + ( evalExpr b )
| MINUS(a, b) -> ( evalExpr a ) - ( evalExpr b )
