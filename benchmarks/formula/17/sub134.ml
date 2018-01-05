type formula =
| TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr =
| NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec eval : formula -> bool = fun x -> (
  let rec expr_eval : expr -> int = function
    | NUM x -> x
    | PLUS (x, y) -> (expr_eval x) + (expr_eval y)
    | MINUS (x, y) -> (expr_eval x) - (expr_eval y)
  in (match x with
    | TRUE -> true
    | FALSE -> false
    | NOT x -> not (eval x)
    | ANDALSO (x, y) -> (eval x) && (eval y)
    | ORELSE (x, y) -> (eval x) || (eval y)
    | IMPLY (x, y) -> (not (eval x)) || (eval y)
    | LESS (x, y) -> (expr_eval x) < (expr_eval y)
  )
)
