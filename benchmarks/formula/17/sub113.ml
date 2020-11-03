(*
    Homework 2, Exercise 1
    2015-15894 Jonghoon Won
    Sep 28, 2017
*)

type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp

and exp = Num of int
         | Plus of exp * exp
         | Minus of exp * exp


let rec eval: formula -> bool =
    let rec evalExpr: exp -> int = fun e ->
        match e with
            | Num n -> n
            | Plus (e1, e2) -> (evalExpr e1) + (evalExpr e2)
            | Minus (e1, e2) -> (evalExpr e1) - (evalExpr e2)
    in

    fun form ->
        match form with
        | True -> true
        | False -> false
        | Not (f) -> not (eval f)
        | AndAlso (f1, f2) -> (eval f1) && (eval f2)
        | OrElse (f1, f2) -> (eval f1) || (eval f2)
        | Imply (f1, f2) -> (not (eval f1)) || (eval f2)
        | Equal (e1, e2) -> (evalExpr e1) = (evalExpr e2)
