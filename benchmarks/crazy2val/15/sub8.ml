type crazy2 = 	NIL
		| ZERO of crazy2
		| ONE of crazy2
		| MONE of crazy2

let rec crazy2val (x: crazy2): int =
	match x with
	| NIL -> 0
	| ZERO NIL -> 0
	| ONE NIL -> 1
	| MONE NIL -> -1
	| ZERO x_post -> 0 + (2 * (crazy2val x_post))
	| ONE x_post -> 1 + (2 * (crazy2val x_post))
	| MONE x_post -> -1 + (2 * (crazy2val x_post))

let _= 
let print_bool x = print_endline (string_of_bool x) in 
print_bool (-1  = (crazy2val (MONE NIL))); 
print_bool (1   = (crazy2val (ONE (ZERO (ZERO (ZERO NIL)))))); 
print_bool (1   = (crazy2val (ONE NIL))); 
print_bool (9   = (crazy2val (MONE (MONE (ONE (ONE (ZERO NIL))))))); 
print_bool (-13 = (crazy2val (MONE (ZERO (ONE (ZERO (ONE (MONE NIL)))))))) 
;; 
