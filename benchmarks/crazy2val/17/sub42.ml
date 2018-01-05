type crazy2 = NIL
	| ZERO of crazy2
	| ONE of crazy2
	| MONE of crazy2

let rec crazy2val: crazy2 -> int = fun c ->
	match c with
	NIL -> 0
	| ZERO(n) -> (crazy2val n) * 2
	| ONE(n) -> (crazy2val n) * 2 + 1
	| MONE(n) -> (crazy2val n) * 2 - 1
