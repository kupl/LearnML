(*Computer Science Engineering 2015-12683 Kim Jaein*)
type crazy2 = NIL 
			| ZERO of crazy2 
			| ONE of crazy2
			| MONE of crazy2

let rec crazy2add ((x:crazy2), (y:crazy2)) =
	match x with
	|NIL ->
		(match y with
		|NIL -> NIL
		| _  -> y)
	|ZERO xnext ->
		(match y with
		|NIL -> x
		|ZERO ynext -> ZERO (crazy2add (xnext, ynext))
		|ONE ynext -> ONE (crazy2add (xnext, ynext))
		|MONE ynext -> MONE (crazy2add (xnext, ynext)))
	|ONE xnext ->
		(match y with
		|NIL -> x
		|ZERO ynext -> ONE (crazy2add (xnext, ynext))
		|ONE ynext->ZERO(crazy2add ((crazy2add(ONE NIL,xnext)),ynext))
		|MONE ynext -> ZERO (crazy2add (xnext, ynext)))
	|MONE xnext ->
		(match y with
		|NIL -> x
		|ZERO ynext -> MONE (crazy2add (xnext, ynext))
		|ONE ynext -> ZERO (crazy2add (xnext, ynext))
		|MONE ynext->ZERO(crazy2add((crazy2add(MONE NIL,xnext)),ynext))
		)

