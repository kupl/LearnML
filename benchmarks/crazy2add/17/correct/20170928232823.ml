type crazy2 = NIL
| ZERO of crazy2
| ONE of crazy2
| MONE of crazy2

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (a,b) ->
match a with
| NIL -> b
| ZERO aa ->
	(match b with
	| NIL -> a
	| ZERO bb -> ZERO(crazy2add(aa, bb))
	| ONE bb -> ONE(crazy2add(aa, bb))
	| MONE bb -> MONE(crazy2add(aa, bb)) )
| ONE aa ->
	(match b with
	| NIL -> a
	| ZERO bb -> ONE(crazy2add(aa, bb))
	| ONE bb -> ZERO(crazy2add(aa, crazy2add(ONE NIL, bb)))
	| MONE bb -> ZERO(crazy2add(aa, bb)) )
| MONE aa ->
	(match b with
	| NIL -> a
	| ZERO bb -> MONE(crazy2add(aa, bb))
	| ONE bb -> ZERO(crazy2add(aa, bb))
	| MONE bb -> ZERO(crazy2add(aa, crazy2add(MONE NIL, bb))) )

