type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add : crazy2*crazy2 -> crazy2 = fun(c1,c2) ->
 match c1 with
 | NIL -> c2
 | ZERO(c3) ->
	(match c2 with
	| NIL -> c1
	| ZERO(c4) -> ZERO(crazy2add(c3,c4))
	| ONE(c4) -> ONE(crazy2add(c3,c4))
	| MONE(c4) -> MONE(crazy2add(c3,c4)))
 | ONE(c3) ->
	(match c2 with
	| NIL -> c1
	| ZERO(c4) -> ONE(crazy2add(c3,c4))
	| ONE(c4) -> ZERO(crazy2add(crazy2add(c3,c4),ONE NIL))
	| MONE(c4) -> ZERO(crazy2add(c3,c4)))
 | MONE(c3) ->
	(match c2 with
	| NIL -> c1
	| ZERO(c4) -> MONE(crazy2add(c3,c4))
	| ONE(c4) -> ZERO(crazy2add(c3,c4))
	| MONE(c4) -> ZERO(crazy2add(crazy2add(c3,c4),MONE NIL)))