(* 4190,310 Programming Language (Fall 2014)
 * Homework 1 - Exercise 2
 * CSE / 2012-13456 / Gao, Chengbin *)

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

let rec eval f =
    match f with
    | TRUE -> true
    | FALSE -> false
    | NOT(a) -> not (eval a)
    | ANDALSO(a, b) -> eval a && eval b
    | ORELSE(a, b) -> eval a || eval b
    | IMPLY(a, b) -> (not (eval a)) || eval b
    | LESS(x, y) -> cal x < cal y

and cal expr =
    match expr with
    | NUM(i) -> i
    | PLUS(x, y) -> cal x + cal y
    | MINUS(x, y) -> cal x - cal y

