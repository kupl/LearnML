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

let rec eval_expr (e : expr): int =
        match e with
        | NUM i -> i
        | PLUS (e1, e2) -> (eval_expr e1)+(eval_expr e2)
        | MINUS (e1, e2) -> (eval_expr e1)-(eval_expr e2)
;;

let rec eval (f: formula) : bool =
        match f with
        | TRUE -> true
        | FALSE -> false
        | NOT f1 -> not (eval f1)
        | ANDALSO (f1, f2) -> (eval f1) && (eval f2)
        | ORELSE (f1, f2) -> (eval f1) || (eval f2)
        | IMPLY (f1, f2) ->
                        (if ((eval f1) &&not (eval f2))then false
                        else true)
        | LESS (e1, e2) -> ((eval_expr e1) - (eval_expr e2)) < 0
;;

