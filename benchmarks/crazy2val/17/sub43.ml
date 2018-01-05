type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val  = fun crazy2 ->
	match crazy2 with
	|NIL -> 0
	|ZERO crazy -> 2 * (crazy2val crazy)
	|ONE crazy -> 1 + 2 * (crazy2val crazy)
	|MONE crazy -> -1 + 2 * (crazy2val crazy)
