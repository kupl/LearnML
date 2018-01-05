(*
    Homework 1, Exercise 2
    2015-15894 Jonghoon Won
    Sep 14, 2017
*)

let rec sigma : int * int * (int -> int) -> int = fun (a, b, f) ->
    if a > b then 0
    else (f a) + sigma (a + 1, b, f)
