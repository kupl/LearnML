(*
	CSE / 2013-11426 / Im DongYeop
	Homework 2: Exercise 3
*)
type crazy2 = NIL
						| ZERO of crazy2
						| ONE of crazy2
						| MONE of crazy2

let rec add((c1: crazy2), (c2: crazy2), (up: int)): crazy2 =
	match up with
	| 1 -> 
		(match c1 with
		| NIL -> 
			(match c2 with
			| NIL -> ONE NIL
			| ZERO cin2 -> ONE cin2 
			| ONE cin2 -> ZERO(add(NIL, cin2, 1))
			| MONE cin2 -> ZERO cin2)
		| ZERO cin1 -> 
			(match c2 with
			| NIL -> ONE cin1
			| ZERO cin2 -> ONE(add(cin1, cin2, 0))
			| ONE cin2 -> ZERO(add(cin1, cin2, 1))
			| MONE cin2 -> ZERO(add(cin1, cin2, 0)))
		| ONE cin1 ->
			(match c2 with
			| NIL -> ZERO(add(cin1, NIL, 1))
			| ZERO cin2 -> ZERO(add(cin1, cin2, 1))
			| ONE cin2 -> ONE(add(cin1, cin2, 1))
			| MONE cin2 -> ONE(add(cin1, cin2, 0)))
		| MONE cin1 ->
			(match c2 with
			| NIL -> ZERO(add(cin1, NIL, 0))
			| ZERO cin2 -> ZERO(add(cin1, cin2, 0))
			| ONE cin2 -> ONE(add(cin1, cin2, 0))
			| MONE cin2 -> MONE(add(cin1, cin2, 0))))
	| 0 -> 
		(match c1 with
		| NIL ->
			(match c2 with
			| NIL -> NIL
			| ZERO cin2 -> ZERO cin2
			| ONE cin2 -> ONE cin2
			| MONE cin2 -> MONE cin2)
		| ZERO cin1 ->
			(match c2 with
			| NIL -> ZERO cin1
			| ZERO cin2 -> ZERO(add(cin1, cin2, 0))
			| ONE cin2 -> ONE(add(cin1, cin2, 0))
			| MONE cin2 -> MONE(add(cin1, cin2, 0)))
		| ONE cin1 ->
			(match c2 with
			| NIL -> ONE cin1
			| ZERO cin2 -> ONE(add(cin1, cin2, 0))
			| ONE cin2 -> ZERO(add(cin1, cin2, 1))
			| MONE cin2 -> ZERO(add(cin1, cin2, 0)))
		| MONE cin1 ->
			(match c2 with
			| NIL -> MONE cin1
			| ZERO cin2 -> MONE(add(cin1, cin2, 0))
			| ONE cin2 -> ZERO(add(cin1, cin2, 0))
			| MONE cin2 -> ZERO(add(cin1, cin2, -1))))
	| _ ->
		(match c1 with
		| NIL ->
			(match c2 with
			| NIL -> MONE NIL
			| ZERO cin2 -> MONE cin2
			| ONE cin2 -> ZERO cin2
			| MONE cin2 -> ZERO(add(NIL, cin2, -1)))
		| ZERO cin1 ->
			(match c2 with
			| NIL -> MONE cin1
			| ZERO cin2 -> MONE(add(cin1, cin2, 0))
			| ONE cin2 -> ZERO(add(cin1, cin2, 0))
			| MONE cin2 -> ZERO(add(cin1, cin2, -1)))
		| ONE cin1 ->
			(match c2 with
			| NIL -> ZERO cin1
			| ZERO cin2 -> ZERO(add(cin1, cin2, 0))
			| ONE cin2 -> ONE(add(cin1, cin2, 0))
			| MONE cin2 -> MONE(add(cin1, cin2, 0)))
		| MONE cin1 ->
			(match c2 with
			| NIL -> ZERO(add(cin1, NIL, -1))
			| ZERO cin2 -> ZERO(add(cin1, cin2, -1))
			| ONE cin2 -> MONE(add(cin1, cin2, 0))
			| MONE cin2 -> MONE(add(cin1, cin2, -1))))

let crazy2add((c1: crazy2), (c2: crazy2)): crazy2 = 
	add(c1, c2, 0)

