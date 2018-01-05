(*
 CSE 2012-11226 Kwak Jin Han
 exercise 2
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

(*
let i = crazy2val (ONE(MONE(ZERO(MONE NIL))))
let _ = print_int i
*)
