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

let rec evalExpr expr =
    match expr with
    | NUM i -> i
    | PLUS (i1, i2) -> (evalExpr i1) + (evalExpr i2)
    | MINUS (i1, i2) -> (evalExpr i1) - (evalExpr i2)

let rec eval formula =
    match formula with
    | TRUE -> true
    | FALSE -> false
    | NOT f -> not (eval f)
    | ANDALSO (f1, f2) -> (eval f1) && (eval f2)
    | ORELSE (f1, f2) -> (eval f1) || (eval f2)
    | IMPLY (p, q) -> (not (eval p)) || (eval q)
    | LESS (e1, e2) -> (evalExpr e1) < (evalExpr e2)
