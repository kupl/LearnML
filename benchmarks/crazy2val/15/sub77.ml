type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2
	
let rec crazy2val (c: crazy2): int = 
	match c with
	| NIL -> 0
	| ONE sc -> 1 + 2*(crazy2val sc)
	| ZERO sc -> 2*(crazy2val sc)
	| MONE sc -> (-1) + 2*(crazy2val sc)
