(*
    Homework 1, Exercise 2
    2015-15894 Jonghoon Won
    Sep 14, 2017
*)

let rec sigma f a b =
    if a > b then 0
    else (f a) + sigma f (a+1) b
