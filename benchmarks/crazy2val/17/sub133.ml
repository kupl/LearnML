type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2val (c : crazy2) = 
	let rec sub (c : crazy2) (n : int) =
		match c with
		| NIL -> 0
		| ZERO c1 -> sub c1 (n * 2)
		| ONE c1 -> n + sub c1 (n * 2)
		| MONE c1 -> -n + sub c1 (n * 2)
	in sub c 1
