type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (a,b)=
match a with
NIL -> b
|ZERO(a2) ->
(
	match b with
	NIL-> a|
	ZERO(b2) -> ZERO(crazy2add (a2,b2))|
	ONE(b2) -> ONE(crazy2add (a2,b2))|
	MONE(b2) -> MONE(crazy2add (a2,b2))
)
|ONE(a2) ->
(	match b with
	NIL -> a|
	ZERO(b2) -> ONE(crazy2add (a2,b2))|
	ONE(b2) -> ZERO(crazy2add ((crazy2add (a2,b2)),(ONE(NIL))))|
	MONE(b2) -> ZERO(crazy2add (a2,b2))
)
|MONE(a2) ->
(	match b with
	NIL -> a|
	ZERO(b2) -> MONE(crazy2add (a2,b2))|
	ONE(b2) -> ZERO(crazy2add (a2,b2))|
	MONE(b2) -> ZERO(crazy2add ((crazy2add (a2,b2)),(MONE(NIL))))
)