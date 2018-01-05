(* SNU Programming Language Fall 2015
 * Homework 2 
 * Exercise 1: crazy2val
 * Written by Dongho Kang 
 *)

type crazy2 = NIL 
| ZERO of crazy2
| ONE  of crazy2 
| MONE of crazy2
;;

let rec crazy2val: crazy2 -> int = fun num ->
    match num with
    | NIL            ->  0   (* Base case: NIL is ZERO *)
    | MONE  num_n    -> -1 + (crazy2val num_n) * 2 (* recursive step case1 *)
    | ZERO  num_n    ->  0 + (crazy2val num_n) * 2 (* recursive step case2 *)
    | ONE   num_n    ->  1 + (crazy2val num_n) * 2 (* recursive step case3 *)
;;
