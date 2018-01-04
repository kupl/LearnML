let rec crazy2val : crazy2 -> int
= fun c ->
	match c with
	| NIL -> 0
	| ZERO a -> 2 * (crazy2val a)
	| ONE a -> 2 * (crazy2val a) + 1
	| MONE a -> 2 * (crazy2val a) - 1

let grading_c2v : crazy2 * crazy2 -> int 
= fun input ->
	crazy2val (crazy2add input)
