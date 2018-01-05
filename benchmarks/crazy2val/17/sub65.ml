(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/2-2.ml *)

type crazy2 = NIL
	    | ZERO of crazy2
	    | ONE of crazy2
	    | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun v ->
  match v with
  | NIL -> 0
  | ZERO tail -> 2 * (crazy2val tail)
  | ONE tail -> 1 + 2 * (crazy2val tail)
  | MONE tail -> -1 + 2 * (crazy2val tail)
