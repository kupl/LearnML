(* 4190,310 Programming Language (Fall 2014)
 * Homework 1 - Exercise 2
 * CSE / 2012-13456 / Gao, Chengbin *)

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

let rec eval f =
    match f with
    | True -> true
    | False -> false
    | Not(a) -> not (eval a)
    | AndAlso(a, b) -> eval a && eval b
    | OrElse(a, b) -> eval a || eval b
    | Imply(a, b) -> (not (eval a)) || eval b
    | Equal(x, y) -> cal x = cal y

and cal exp =
    match exp with
    | Num(i) -> i
    | Plus(x, y) -> cal x + cal y
    | Minus(x, y) -> cal x - cal y

