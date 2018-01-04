type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2
exception E

let rec crazy2add (a, b) =
	let rec add (a, b) =
		match (a, b) with
		(ZERO c, ZERO d) -> ZERO(crazy2add(c, d))
		|(ONE c, MONE d) -> ZERO(crazy2add(c, d))
		|(MONE c, ONE d) -> ZERO(crazy2add(c, d))
		| (ZERO c, ONE d) ->ONE(crazy2add(c, d))
		| (ONE c, ZERO d) -> ONE(crazy2add(c, d))
		| (ZERO c, MONE d) -> MONE(crazy2add(c, d))
		| (MONE c, ZERO d) -> MONE(crazy2add(c, d))
		| (ONE c, ONE d) -> ZERO(crazy2add(ONE(NIL),(crazy2add(c, d))))
		| (MONE c, MONE d) -> ZERO(crazy2add(MONE(NIL),(crazy2add(c, d))))
		| (NIL, b) -> b
		| (a, NIL) -> a in
	
	let rec trim a =
		match a with
		NIL -> NIL
		|ONE b -> ONE(trim b)
		|MONE b -> MONE(trim b)
		|ZERO b ->
			match b with
			NIL -> NIL
			|ONE c -> ZERO(trim b)
			|MONE c -> ZERO(trim b)
			|ZERO c -> trim(ZERO(trim b)) in
	
	let rec zero a =
		match a with
		NIL -> ZERO NIL
		|_ -> a in
	
	zero(trim(add(a, b)))
