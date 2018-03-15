type crazy2 =
	| NIL
	| ZERO of crazy2
	| ONE of crazy2
	| MONE of crazy2

let rec crazy2add : (crazy2 * crazy2) -> crazy2
= fun (c1, c2) ->
	match c1 with
	| NIL -> c2
	| ZERO c1' ->
		begin match c2 with
		| NIL -> c1
		| ZERO c2' -> ZERO (crazy2add (c1', c2'))
		| ONE c2' -> ONE (crazy2add (c1', c2'))
		| MONE c2' -> MONE (crazy2add (c1', c2'))
		end
	| ONE c1' ->
		begin match c2 with
		| NIL -> c1
		| ZERO c2' -> ONE (crazy2add (c1', c2'))
		| ONE c2' -> ZERO (crazy2add ((ONE NIL), crazy2add (c1', c2')))
		| MONE c2' -> ZERO (crazy2add (c1', c2'))
		end
	| MONE c1' ->
		begin match c2 with
		| NIL -> c1
		| ZERO c2' -> MONE (crazy2add (c1', c2'))
		| ONE c2' -> ZERO (crazy2add (c1', c2'))
		| MONE c2' -> ZERO (crazy2add ((MONE NIL), crazy2add (c1', c2')))
		end