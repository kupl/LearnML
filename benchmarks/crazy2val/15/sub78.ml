(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 2, Exercise 1 *)

(* type crazy2 *)
type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

(* crazy2val *)
let crazy2val cz =
    let rec add = fun cz value digit ->
        match cz with
        | NIL -> value
        | ZERO subcz -> add subcz value (digit * 2)
        | ONE subcz -> add subcz (value + digit) (digit * 2)
        | MONE subcz -> add subcz (value - digit) (digit * 2)
    in
    add cz 0 1
