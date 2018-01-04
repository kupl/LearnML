(* SNU Programming Language Fall 2015
 * Homework 1 
 * Exercise 5: natadd & natmul
 * Written by Dongho Kang 
 *)

type nat = ZERO | SUCC of nat;;

let rec natadd : nat * nat -> nat = fun (a, b) ->
    match b with
    | ZERO -> a 
    | SUCC b_1 -> SUCC (natadd (a, b_1))
;;

let rec natmul : nat * nat -> nat = fun (a, b) ->
    match b with
    | ZERO -> ZERO
    | SUCC b_1 -> natadd (a, natmul(a, b_1))
;;
