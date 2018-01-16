type crazy2  = NIL
    | ZERO of crazy2
    | ONE of crazy2
    | MONE of crazy2

let rec powerof2 = function
    | 0 -> 1
    | 1 -> 2
    | n ->(powerof2(n-1)) * 2

let rec foo k = function
    | NIL -> 0
    | ZERO next -> foo(k+1) next
    | ONE next -> foo(k+1) next + powerof2(k)
    | MONE next -> foo(k+1) next - powerof2(k)

let rec crazy2val = foo 0

let rec crazy2add (c1, c2) =
	match (c1, c2) with
	| (NIL, _) -> c2
	| (_, NIL) -> c1
	| (ZERO n1, ZERO n2) | (ONE n1, MONE n2) | (MONE n1, ONE n2) -> ZERO(crazy2add(n1,n2))
	| (ZERO n1, ONE n2) | (ONE n1, ZERO n2) -> ONE(crazy2add(n1,n2))
	| (ZERO n1, MONE n2) | (MONE n1, ZERO n2) -> MONE(crazy2add(n1,n2))
	| (ONE n1, ONE n2) -> ZERO(crazy2add(crazy2add(n1,ONE(NIL)), n2))
	| (MONE n1, MONE n2) -> ZERO(crazy2add(crazy2add(n1,MONE(NIL)), n2))