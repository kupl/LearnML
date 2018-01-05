exception TODO

type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2val (c:crazy2): int =
	match c with
	| NIL -> 0
	| (ZERO a) -> 2*(crazy2val a)
	| (ONE a) -> (2*(crazy2val a)+1)
	| (MONE a) -> (2*(crazy2val a)-1)


