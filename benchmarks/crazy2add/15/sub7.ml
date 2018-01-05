exception TODO

type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2length (x: crazy2): int =
	match x with
	| NIL -> 0
	| MONE x_sub -> 1 + (crazy2length x_sub)
	| ZERO x_sub -> 1 + (crazy2length x_sub)
	| ONE x_sub -> 1 + (crazy2length x_sub)

let rec addzero (x: crazy2): crazy2 = 
	match x with
	| NIL -> ZERO NIL
	| MONE x_sub -> MONE (addzero x_sub)
	| ZERO x_sub -> ZERO (addzero x_sub)
	| ONE x_sub -> ONE (addzero x_sub)

let rec crazy2match (x: crazy2) (y: crazy2): crazy2 list =
	if (crazy2length x) = (crazy2length y)
		then [x ; y]
	else if (crazy2length x) > (crazy2length y)
		then (crazy2match x (addzero y))
	else (crazy2match (addzero x) y)

let rec crazy2add (a: crazy2) (b: crazy2): crazy2 =	
	let rec dec (x: crazy2): crazy2 = 
		match x with
		| NIL -> MONE NIL
		| ZERO x_sub -> MONE x_sub
		| ONE x_sub -> ZERO x_sub
		| MONE x_sub -> ZERO (dec x_sub) in
	let rec inc (x: crazy2): crazy2 =
		match x with
		| NIL -> ONE NIL
		| ZERO x_sub -> ONE x_sub
		| MONE x_sub -> ZERO x_sub
		| ONE x_sub -> ZERO (inc x_sub) in
	let rec crazy2add_length (a: crazy2) (b: crazy2): crazy2 =
		match (a, b) with
		| (NIL, NIL) -> NIL
		| (_, NIL) -> a
		| (NIL, _) -> b
		| (MONE a_sub, MONE b_sub) -> ZERO (crazy2add_length a_sub (dec b_sub))
		| (MONE a_sub, ZERO b_sub) -> MONE (crazy2add_length a_sub b_sub)
		| (MONE a_sub, ONE b_sub) -> ZERO (crazy2add_length a_sub b_sub)
		| (ZERO a_sub, MONE b_sub) -> MONE (crazy2add_length a_sub b_sub)
		| (ZERO a_sub, ZERO b_sub) -> ZERO (crazy2add_length a_sub b_sub)
		| (ZERO a_sub, ONE b_sub) -> ONE (crazy2add_length a_sub b_sub)
		| (ONE a_sub, MONE b_sub) -> ZERO (crazy2add_length a_sub b_sub)
		| (ONE a_sub, ZERO b_sub) -> ONE (crazy2add_length a_sub b_sub)
		| (ONE a_sub, ONE b_sub) -> ZERO (crazy2add_length a_sub (inc b_sub)) in
	let new_list = (crazy2match a b) in
	(crazy2add_length (List.nth new_list 0) (List.nth new_list 1))
