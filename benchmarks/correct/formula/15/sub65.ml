(* 2014-18790 JangHo Seo =jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 1, Exercise 4 *)

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

let rec evalExpr exp =
    match exp with
    | Num i -> i
    | Plus (i1, i2) -> (evalExpr i1) + (evalExpr i2)
    | Minus (i1, i2) -> (evalExpr i1) - (evalExpr i2)

let rec eval formula =
    match formula with
    | True -> true
    | False -> false
    | Not f -> not (eval f)
    | AndAlso (f1, f2) -> (eval f1) && (eval f2)
    | OrElse (f1, f2) -> (eval f1) || (eval f2)
    | Imply (p, q) -> (not (eval p)) || (eval q)
    | Equal (e1, e2) -> (evalExpr e1) = (evalExpr e2)
