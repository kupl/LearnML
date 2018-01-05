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

let rec value : expr -> int = fun l ->
 match l with
 | NUM n -> n
 | PLUS(m,n) -> (value m) + (value n)
 | MINUS(m,n) -> (value m) - (value n)

let rec eval : formula -> bool = fun form ->
 match form with
 | TRUE -> true
 | FALSE -> false
 | NOT x -> not (eval x)
 | ANDALSO (x,y) -> (eval x)&&(eval y)
 | ORELSE (x,y) -> (eval x)||(eval y)
 | IMPLY (x,y) -> (not (eval x))||(eval y)
 | LESS (x,y) -> (value x)<(value y)