type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

exception Error of string

let crazy2add (a,b)=

let rec crazy2add_internal (a,b)=
match a with
NIL -> b
|ZERO(a2) ->
(
	match b with
	NIL-> a|
	ZERO(b2) -> ZERO(crazy2add_internal (a2,b2))|
	ONE(b2) -> ONE(crazy2add_internal (a2,b2))|
	MONE(b2) -> MONE(crazy2add_internal (a2,b2))
)
|ONE(a2) ->
(	match b with
	NIL -> a|
	ZERO(b2) -> ONE(crazy2add_internal (a2,b2))|
	ONE(b2) -> ZERO(crazy2add_internal ((crazy2add_internal (a2,b2)),(ONE(NIL))))|
	MONE(b2) -> ZERO(crazy2add_internal (a2,b2))
)
|MONE(a2) ->
(	match b with
	NIL -> a|
	ZERO(b2) -> MONE(crazy2add_internal (a2,b2))|
	ONE(b2) -> ZERO(crazy2add_internal (a2,b2))|
	MONE(b2) -> ZERO(crazy2add_internal ((crazy2add_internal (a2,b2)),(MONE(NIL))))
)
in

let rec crazy2add_reduceZero_internal n=
match n with
NIL->NIL|
ZERO(NIL)->NIL|
ZERO(b)->ZERO(crazy2add_reduceZero_internal (crazy2add_reduceZero_internal b) )|
ONE(b)->ONE(crazy2add_reduceZero_internal b)|
MONE(b)->MONE(crazy2add_reduceZero_internal b)
in

let crazy2add_reduceZero n=
let x=crazy2add_reduceZero_internal n in
match x with
NIL->ZERO(NIL)|
_->x
in

if a=NIL or b=NIL then
raise (Error "NIL is not number")
else
crazy2add_reduceZero(crazy2add_internal (a,b))