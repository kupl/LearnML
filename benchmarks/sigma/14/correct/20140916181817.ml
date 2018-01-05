(* 4190,310 Programming Language (Fall 2014)
 * Homework 1 - Exercise 1
 * CSE / 2012-13456 / Gao, Chengbin *)

let rec sigma (a, b, f) =
    if a > b then 0
    else sigma (a + 1, b, f) + f a
