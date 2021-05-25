(* 2012-11230 Kim sangmin *)

type crazy2 = NIL
			| ZERO of crazy2
			| ONE of crazy2
			| MONE of crazy2

let rec crazy2add : crazy2*crazy2 -> crazy2 = fun (x,y) ->
	match x,y with
	| (NIL, _) -> y
	| (_, NIL) -> x
	| (ZERO(i), ZERO(j)) -> ZERO(crazy2add(i,j))
	| (ZERO(i), ONE(j)) -> ONE(crazy2add(i,j))
	| (ZERO(i), MONE(j)) -> MONE(crazy2add(i,j))
	| (ONE(i), ZERO(j)) -> ONE(crazy2add(i,j))
	| (ONE(i), ONE(j)) -> ZERO(crazy2add(ONE(NIL), crazy2add(i,j)))
	| (ONE(i), MONE(j)) -> ZERO(crazy2add(i,j))
	| (MONE(i), ZERO(j)) -> MONE(crazy2add(i,j))
	| (MONE(i), ONE(j)) -> ZERO(crazy2add(i,j))
	| (MONE(i), MONE(j)) -> ZERO(crazy2add(MONE(NIL), crazy2add(i,j)))

