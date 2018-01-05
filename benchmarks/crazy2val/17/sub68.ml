type crazy2 = NIL
				| ZERO of crazy2
				| ONE of crazy2
				| MONE of crazy2

let rec crazy2val (c:crazy2) : int = 
	 match c with
		  |NIL -> 0
		  |ONE c1 -> 1 + 2 * crazy2val(c1)
		  |ZERO c1 -> 0 + 2 * crazy2val(c1)
		  |MONE c1 -> -1 + 2 * crazy2val(c1)
(*
let c2 : crazy2 = ZERO(ONE(MONE NIL)) (*-2*)
let c3 : crazy2 = MONE(ONE(ONE(ZERO(MONE NIL)))) (*-11*)
let _ = if crazy2val c2 == -2 then print_endline ("O")
		  else print_endline ("X")
let _ = if crazy2val c3 == -11 then print_endline ("O")
		  else print_endline ("X")
*)
