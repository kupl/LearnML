exception Error of string

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val c = match c with
  			ZERO NIL -> 0
			|NIL -> raise(Error "Invalid arg")
  			|ONE NIL -> 1
  			|MONE NIL -> -1
  			|ZERO(b) -> (crazy2val b) * 2
  			|ONE(b) -> (crazy2val b) * 2 + 1
  			|MONE(b) -> (crazy2val b) * 2 - 1;;