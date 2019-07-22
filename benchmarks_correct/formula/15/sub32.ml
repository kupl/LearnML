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
;;
  
let rec eval form =
(*val eval : formula -> bool*)
  let rec expreval expr =
    (*val expreval : expr -> int*)
    match expr with
    | PLUS(e1,e2) -> ((expreval e1) + (expreval e2))
    | MINUS(e1,e2) -> ((expreval e1) - (expreval e2))
    | NUM(i) -> i in
  match form with
  | NOT(f) -> (not (eval f))
  | ANDALSO(f1,f2) -> ((eval f1) && (eval f2))
  | ORELSE(f1,f2) -> ((eval f1) || (eval f2))
  | IMPLY(f1,f2) -> ((not (eval f1)) || (eval f2))
  | LESS(e1, e2) -> ((expreval e1) < (expreval e2))
  | TRUE -> true
  | FALSE -> false;;
