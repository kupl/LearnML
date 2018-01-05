(*
    Homework 2, Exercise 2
    2015-15894 Jonghoon Won
    Sep 28, 2017
*)

type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val: crazy2 -> int = fun expr ->
    match expr with
    | NIL -> 0
    | ZERO (e) -> 2 * (crazy2val e)
    | ONE (e) -> 1 + (2 * (crazy2val e))
    | MONE (e) -> (-1) + (2 * (crazy2val e))
