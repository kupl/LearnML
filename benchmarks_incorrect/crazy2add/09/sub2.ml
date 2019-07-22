type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add(a, b) = 
	let rec adder(a, b) =
	match a with 
		ZERO aa ->
		(match b with 
			ZERO bb -> ZERO(adder(aa, bb))
			|ONE bb -> ONE (adder(aa, bb))
			|MONE bb -> MONE(adder(aa, bb))
			|NIL -> ZERO(aa)
		)

		|ONE aa ->
		(match b with 
			ZERO bb -> ONE(adder(aa, bb))
			|ONE bb -> ZERO(adder(adder(aa, ONE NIL), bb))
			|MONE bb -> ZERO(adder(aa, bb))
			|NIL -> ONE(aa)
		)	
	
		|MONE aa ->
		(match b with
			ZERO bb -> MONE(adder(aa, bb))
			|ONE bb -> ZERO(adder(aa, bb))
			|MONE bb -> ZERO(adder(adder(aa, ONE NIL), bb))
			|NIL -> MONE(aa)
		)

		|NIL -> 
		(match b with 
	 		ZERO bb -> ZERO(bb)
			|ONE bb -> ONE(bb)
			|MONE bb -> MONE(bb)
			|NIL -> NIL
		) in
	

	let rec rid a =
	match a with
		NIL -> NIL
		|ONE b -> ONE(rid b)
		|MONE b -> MONE(rid b)
		|ZERO b ->
			match b with
			NIL -> NIL
			|ONE aa -> ZERO(rid b)
			|MONE aa -> ZERO(rid b)
			|ZERO aa -> rid(ZERO(rid b)) in

let rec zero a =
	match a with
	NIL -> ZERO NIL
	|_ -> a in

zero(rid(adder(a, b)))
