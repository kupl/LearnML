(*
    Homework 1, Exercise 4
    2015-15894 Jonghoon Won
    Sep 14, 2017
*)

type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (n1, n2) ->
    match (n1, n2) with
    | (ZERO, _) -> n2
    | (_, ZERO) -> n1
    | (_, SUCC (tl)) -> natadd (SUCC (n1), tl)

let rec natmul : nat * nat -> nat = fun (n1, n2) ->
    match (n1, n2) with
    | (ZERO, _) -> ZERO
    | (_, ZERO) -> ZERO
    | (_, SUCC (tl)) -> natadd (n1, natmul (n1, tl))
