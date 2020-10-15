(* 4190,310 Programming Language (Fall 2014)
 * Homework 1 - Exercise 1
 * CSE / 2012-13456 / Gao, Chengbin *)

let rec sigma f a b  =
    if a = b then f a
    else sigma f (a+1) b + f a
