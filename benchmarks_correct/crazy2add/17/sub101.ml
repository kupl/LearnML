(* 2015-11380 박찬양 HW2_3 *)

type crazy2 = NIL 
            | ZERO of crazy2
						| ONE of crazy2
						| MONE of crazy2

type crazy22 = Z | O | M

let crazy2add: crazy2 * crazy2 -> crazy2 = fun(cra1, cra2) ->
	let rec addadd(a, b, c) =	
		match (a, b, c) with
		| (NIL, NIL, Z) -> NIL
		| (NIL, NIL, O) -> ONE NIL
		| (NIL, NIL, M) -> MONE NIL
		| (ONE n, ONE m, O) -> ONE (addadd(n, m, O))
		| (ONE n, ONE m, Z)|(ONE n, ZERO m, O)|(ZERO n, ONE m, O) -> ZERO (addadd(n, m, O))		
		| (ONE n, ONE m, M)|(ONE n, ZERO m, Z)|(ONE n, MONE m, O)|(ZERO n, ONE m, Z)|(ZERO n, ZERO m, O)|(MONE n, ONE m, O) -> ONE (addadd(n, m, Z)) 
		| (ONE n, ZERO m, M)|(ONE n, MONE m, Z)|(ZERO n, ONE m, M)|(ZERO n, ZERO m, Z)|(ZERO n, MONE m, O)|(MONE n, ONE m, Z)|(MONE n, ZERO m, O) -> ZERO (addadd(n, m, Z))
		| (ONE n, MONE m, M)|(ZERO n, ZERO m, M)|(ZERO n, MONE m, Z)|(MONE n, ONE m, M)|(MONE n, ZERO m, Z)|(MONE n, MONE m, O) -> MONE (addadd(n, m, Z))
		| (ZERO n, MONE m, M)|(MONE n, ZERO m, M)|(MONE n, MONE m, Z) -> ZERO (addadd (n, m, M))
		| (MONE n, MONE m, M) -> MONE (addadd(n, m, M))
		| (NIL, m, O)|(m, NIL, O) -> addadd(ONE NIL, m, Z)
		| (NIL, m, M)|(m, NIL, M) -> addadd(MONE NIL, m, Z)
		| (NIL, m, Z)|(m, NIL, Z) -> m
	in addadd(cra1, cra2, Z)
