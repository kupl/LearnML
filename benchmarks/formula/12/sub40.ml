﻿type formula = TRUE
        | FALSE
        | NOT of formula
        | ANDALSO of formula * formula
        | ORELSE of formula * formula
        | IMPLY of formula * formula
        | LESS of expr * expr

and expr = NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr

let rec eval form =
        let rec etoint e =
                match e with
                | NUM a -> a
                | PLUS (e1, e2) -> (etoint e1) + (etoint e2)
                | MINUS (e1, e2) -> (etoint e1) - (etoint e2)
        in true

        match form with
        | TRUE -> true
        | FALSE -> false
        | NOT form -> (not (eval form))
        | ANDALSO (f1, f2) -> (eval f1) && (eval f2)
        | ORELSE (f1, f2) -> (eval f1) || (eval f2)
        | IMPLY  (f1, f2) -> (not (eval f1)) || (eval f2)
        | LESS (e1, e2) -> ((etoint e1) < (etoint e2))
