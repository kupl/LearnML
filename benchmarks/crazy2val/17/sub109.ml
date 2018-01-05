(* 2015-1478 Giyeon Kim HW 2 *)

(* Exercise 2 *)
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val: crazy2 -> int = fun cnum ->
    match cnum with
    | NIL -> 0
    | ZERO tl -> 2 * crazy2val tl
    | ONE t1 -> 1 + 2 * crazy2val t1
    | MONE t1 -> -1 + 2 * crazy2val t1

