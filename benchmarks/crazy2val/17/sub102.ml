type crazy2 = NIL
						| ZERO of crazy2
						| ONE of crazy2
						| MONE of crazy2

let crazy2val crazy =
	let rec crazyOne crazy =
		match crazy with
		| NIL -> 0
		| ZERO (a) -> crazyOne (a) * 2
		| ONE (a) -> crazyOne(a) * 2 + 1 
		| MONE (a) -> crazyOne(a) * 2 - 1 in
	crazyOne crazy


