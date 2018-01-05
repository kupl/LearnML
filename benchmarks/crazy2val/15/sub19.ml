type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2val crazy =
	let rec eval a b = 
	match (a,b) with
	| (NIL,_) -> 0
	| (ZERO a,b) -> eval a (b+1)
	| (ONE a,b) -> 1*(int_of_float(2.**float_of_int(b))) + (eval a (b+1))
	| (MONE a,b) -> (-1)*(int_of_float(2.**float_of_int(b))) + (eval a (b+1))
	in
	eval crazy 0
	