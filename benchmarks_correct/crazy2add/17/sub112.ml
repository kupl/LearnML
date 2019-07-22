(*
    Homework 2, Exercise 3
    2015-15894 Jonghoon Won
    Sep 28, 2017
*)

type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (expr1, expr2) ->
    match (expr1, expr2) with
    | (NIL, _) -> expr2
    | (_, NIL) -> expr1

    | (ONE (e1), ONE (e2)) -> ZERO (crazy2add ((ONE (NIL)), crazy2add (e1, e2)))
    | (MONE (e1), MONE (e2)) -> ZERO (crazy2add ((MONE (NIL)), crazy2add (e1, e2)))

    | (ZERO (e1), ZERO (e2))
    | (MONE (e1), ONE (e2))
    | (ONE (e1), MONE (e2)) -> ZERO (crazy2add (e1, e2))

    | (ONE (e1), ZERO (e2))
    | (ZERO (e1), ONE (e2)) -> ONE (crazy2add (e1, e2))

    | (ZERO (e1), MONE (e2))
    | (MONE (e1), ZERO (e2)) -> MONE (crazy2add (e1, e2))
