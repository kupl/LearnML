(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 2, Exercise 2 *)

(* type crazy2 *)
type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

(* plus one *)
let rec crazy2one cz =
    match cz with
    | NIL -> (ONE NIL)
    | ZERO subcz -> (ONE subcz)
    | ONE subcz -> (ZERO (crazy2one subcz))
    | MONE subcz -> (ZERO subcz)

(* minus one *)
let rec crazy2mone cz =
    match cz with
    | NIL -> (MONE NIL)
    | ZERO subcz -> (MONE subcz)
    | ONE subcz -> (ZERO subcz)
    | MONE subcz -> (ZERO (crazy2mone subcz))

(* crazy2add *)
let rec crazy2add (a, b) =
    match (a, b) with
    | (NIL, _) -> b
    | (_, NIL) -> a
    | (ONE  sa, ONE  sb) -> (ZERO (crazy2add ((crazy2one sa), sb)))
    | (ONE  sa, ZERO sb) -> (ONE (crazy2add (sa, sb)))
    | (ONE  sa, MONE sb) -> (ZERO (crazy2add (sa, sb)))
    | (ZERO sa, ONE  sb) -> (ONE (crazy2add (sa, sb)))
    | (ZERO sa, ZERO sb) -> (ZERO (crazy2add (sa, sb)))
    | (ZERO sa, MONE sb) -> (MONE (crazy2add (sa, sb)))
    | (MONE sa, ONE  sb) -> (ZERO (crazy2add (sa, sb)))
    | (MONE sa, ZERO sb) -> (MONE (crazy2add (sa, sb)))
    | (MONE sa, MONE sb) -> (ZERO (crazy2add ((crazy2mone sa), sb)))
