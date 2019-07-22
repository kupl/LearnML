type crazy2 = NIL
				| ZERO of crazy2
				| ONE of crazy2
				| MONE of crazy2
(*
let c2 : crazy2 = ZERO(ONE(MONE NIL)) (*-2*)
let c3 : crazy2 = MONE(ONE(ONE(ZERO(MONE NIL)))) (*-11*)
let _ = if crazy2val c2 == -2 then print_endline ("O")
		  else print_endline ("X")
let _ = if crazy2val c3 == -11 then print_endline ("O")
		  else print_endline ("X")
*)

let rec crazy2add( (c1:crazy2),(c2:crazy2) ) : crazy2 =
	 match c1 with
	 |NIL -> c2
	 |ZERO c1_t -> (match c2 with
						  |NIL -> c1
						  |ZERO c2_t -> ZERO( crazy2add(c1_t,c2_t))
						  |ONE c2_t -> ONE( crazy2add(c1_t,c2_t))
						  |MONE c2_t -> MONE( crazy2add(c1_t,c2_t))
						  )
	 |ONE c1_t -> (match c2 with
						  |NIL -> c1
						  |ZERO c2_t -> ONE(crazy2add(c1_t,c2_t))
						  |ONE c2_t -> ZERO(crazy2add(ONE NIL, crazy2add(c1_t,c2_t)))
						  |MONE c2_t -> ZERO(crazy2add(c1_t,c2_t))
						  )
	 |MONE c1_t -> (match c2 with
						  |NIL -> c1
						  |ZERO c2_t -> MONE(crazy2add(c1_t,c2_t))
						  |ONE c2_t -> ZERO(crazy2add(c1_t,c2_t))
						  |MONE c2_t -> ZERO(crazy2add(MONE NIL, crazy2add(c1_t,c2_t)))
						  )


(*
let c2 : crazy2 = ZERO(ONE(MONE NIL)) (*-2*)
let c3 : crazy2 = MONE(ONE(ONE(ZERO(MONE NIL)))) (*-11*)
let c4 : crazy2 = crazy2add (c2,c3) (*-13*)

let _ = if crazy2val c4 == -13 then print_endline ("O")
		  else print_endline ("X")
*)

