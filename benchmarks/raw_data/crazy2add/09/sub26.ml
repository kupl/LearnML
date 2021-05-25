
(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)

(* 6.ml *)
exception Error of string

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2add (x, y) =
  let trim_zero a =
    if a = (ZERO NIL) then
      ZERO NIL
    else 
      ZERO a
  in

  let rec c2a (x, y) =
    match (x, y) with 
        _, NIL -> x
      | NIL, _ -> y
      | ZERO xs, ONE ys | ONE xs, ZERO ys -> ONE (c2a (xs, ys))
      | ZERO xs, MONE ys | MONE xs, ZERO ys -> MONE (c2a (xs, ys))

      | ONE xs, ONE ys -> trim_zero (c2a ((c2a (xs, (ONE NIL))), ys))
      | MONE xs, MONE ys -> trim_zero (c2a ((c2a (xs, (MONE NIL))), ys))
      | ONE xs, MONE ys | MONE xs, ONE ys | ZERO xs, ZERO ys -> trim_zero (c2a (xs, ys))

  in
    match (x, y) with
	_, NIL | NIL, _ -> raise (Error "NIL is not valid")
      | _, _ -> c2a (x, y)
