(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/2-3.ml *)

type crazy2 = NIL
	    | ZERO of crazy2
	    | ONE of crazy2
	    | MONE of crazy2

(*
let rec crazy2val : crazy2 -> int = fun v ->
  match v with
  | NIL -> 0
  | ZERO tail -> 2 * (crazy2val tail)
  | ONE tail -> 1 + 2 * (crazy2val tail)
  | MONE tail -> -1 + 2 * (crazy2val tail)
*)

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (a, b) ->
  match (a, b) with
  | (NIL, _) -> b

  | (ZERO m, b) ->
      (
      match b with
      | NIL -> ZERO m
      | ZERO n -> ZERO (crazy2add (m, n))
      | ONE n -> ONE (crazy2add (m, n))
      | MONE n -> MONE (crazy2add (m, n))
      )

  | (ONE m, b) ->
      (
      match b with
      | NIL -> ONE m
      | ZERO n -> ONE (crazy2add (m, n))
      | ONE n -> ZERO (crazy2add (ONE NIL, crazy2add (m, n)))
      | MONE n -> ZERO (crazy2add (m, n))
      )

  | (MONE m, b) ->
      (
      match b with
      | NIL -> MONE m
      | ZERO n -> MONE (crazy2add (m, n))
      | ONE n -> ZERO (crazy2add (m, n))
      | MONE n -> ZERO (crazy2add (MONE NIL, crazy2add (m, n)))
      )
