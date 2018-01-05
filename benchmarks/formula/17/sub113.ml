(*
    Homework 2, Exercise 1
    2015-15894 Jonghoon Won
    Sep 28, 2017
*)

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


let rec eval: formula -> bool =
    let rec evalExpr: expr -> int = fun e ->
        match e with
            | NUM n -> n
            | PLUS (e1, e2) -> (evalExpr e1) + (evalExpr e2)
            | MINUS (e1, e2) -> (evalExpr e1) - (evalExpr e2)
    in

    fun form ->
        match form with
        | TRUE -> true
        | FALSE -> false
        | NOT (f) -> not (eval f)
        | ANDALSO (f1, f2) -> (eval f1) && (eval f2)
        | ORELSE (f1, f2) -> (eval f1) || (eval f2)
        | IMPLY (f1, f2) -> (not (eval f1)) || (eval f2)
        | LESS (e1, e2) -> (evalExpr e1) < (evalExpr e2)
