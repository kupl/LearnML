(*
 * Programming Languages, 2013 Fall.
 * Solution for Homework 1.
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

(* Exercise 1: sigma *)
let rec sigma f a b =
  if a > b then 0
  else
    (f a) + (sigma f (a+1) b)