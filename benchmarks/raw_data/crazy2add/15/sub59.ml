type crazy2 = NIL 
	| ZERO of crazy2 
	| ONE of crazy2 
	| MONE of crazy2

let rec crazy2val = fun a ->
	match a with
	|NIL -> 0
	|ZERO b -> 2*(crazy2val b) 
	|ONE b -> 1 + 2*(crazy2val b)
	|MONE b -> -1 + 2*(crazy2val b)

let rec crazy2add = fun (a, b) ->
	match a with 
	|NIL -> b
	|ZERO at ->
		(match b with
		|NIL -> a
		|ZERO bt -> ZERO(crazy2add (at, bt))
		|ONE bt -> ONE(crazy2add (at, bt))
		|MONE bt -> MONE(crazy2add (at, bt))
		)
	|ONE at ->
		(match b with
		|NIL -> a
		|ZERO bt -> ONE(crazy2add (at, bt))
		|ONE bt -> ZERO(crazy2add (crazy2add((ONE NIL), at), bt))
		|MONE bt -> ZERO(crazy2add (at, bt))
		)
	|MONE at ->
		(match b with
		|NIL -> a
		|ZERO bt -> MONE(crazy2add (at, bt))
		|ONE bt -> ZERO(crazy2add (at, bt))
		|MONE bt -> ZERO(crazy2add (crazy2add((MONE NIL), at), bt))
		)


