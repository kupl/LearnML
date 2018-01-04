(*
 CSE 2012-11226 Kwak Jin Han
 exercise 3
 *)

type crazy2 = NIL
						| ZERO of crazy2
						| ONE of crazy2
						| MONE of crazy2

(* crazy2val : crazy2 -> int *)						
let rec crazy2val f =
	match f with
	| NIL -> 0
	| ZERO pf -> 0 + (crazy2val pf) * 2
	| ONE pf -> 1 + (crazy2val pf) * 2
	| MONE pf -> -1 + (crazy2val pf) * 2

(* crazy2add : crazy2 * crazy2 -> crazy2 *)
let rec crazy2add (left, right) = 
				match (left, right) with
(* 1 *)	| (NIL, NIL) -> NIL
(* 3 *)	| (NIL, _) -> right
(* 3 *)	| (_, NIL) -> left
(* 1 *)	| (ZERO lf, ZERO rf) -> ZERO (crazy2add (lf, rf))
(* 1 *) | (ZERO lf, ONE rf) -> ONE (crazy2add (lf, rf))
(* 1 *) | (ZERO lf, MONE rf) -> MONE (crazy2add (lf, rf))
(* 1 *) | (ONE lf, ZERO rf) -> ONE (crazy2add (lf, rf))
(* 1 *) | (ONE lf, ONE rf) -> crazy2add ((ZERO (ONE NIL)), ZERO (crazy2add (lf, rf)))
(* 1 *) | (ONE lf, MONE rf) -> ZERO (crazy2add (lf, rf))
(* 1 *) | (MONE lf, ZERO rf) -> MONE (crazy2add (lf, rf))
(* 1 *) | (MONE lf, ONE rf) -> ZERO (crazy2add (lf, rf))
(* 1 *) | (MONE lf, MONE rf) -> crazy2add ((ZERO (MONE NIL)), ZERO (crazy2add (lf, rf)))

(*
let x = ZERO (MONE (ONE NIL))
let y = ONE (ONE (ONE NIL))

let i = crazy2val (crazy2add (x, y))
let _ = print_int i

let j = crazy2val x + crazy2val y
let _ = print_int j
*)
