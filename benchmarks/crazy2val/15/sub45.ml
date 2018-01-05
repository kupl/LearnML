type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val crazy2 =
	match crazy2 with
	| NIL -> 0
	| ZERO c -> 0 + 2*(crazy2val c)
	| ONE c -> 1 + 2*(crazy2val c)
	| MONE c -> -1 + 2*(crazy2val c)
;;
