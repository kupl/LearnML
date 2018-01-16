(*
 * Programming Languages, 2013 Fall.
 * Solution for Homework 1.
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

(* Exercise 1: sigma *)
let rec sigma (a, b, f) =
  if a > b then 0
  else
    (f a) + (sigma (a+1, b, f))
