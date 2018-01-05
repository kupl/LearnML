(*
 * Homework 1 - Exercise 1
 * 2011-10492 Jaeyeong Yang
 *)
let rec merge: int list * int list -> int list = fun (a, b) ->
  match a with
  | [] -> b
  | ha :: ta ->
      (match b with
      | [] -> a
      | hb :: tb ->
          if ha > hb then ha :: merge (ta, b)
          else hb :: merge(a, tb))

