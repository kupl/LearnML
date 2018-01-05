type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2
	
let rec pc (a: crazy2): crazy2 =
	match a with
	| NIL -> ONE NIL
	| ZERO c -> ONE c
	| MONE c -> ZERO c
	| ONE c -> ZERO (pc c)	
	
let rec mc (a: crazy2): crazy2 =
	match a with
	| NIL -> MONE NIL
	| ZERO c -> MONE c
	| ONE c -> ZERO c
	| MONE c -> ZERO (mc c)
	
let rec crazy2add ((a: crazy2), (b: crazy2)): crazy2 =
  match (a, b) with
	| (NIL, NIL) -> NIL
	| (NIL, _) -> b
	| (_, NIL) -> a
	| (ZERO ca, ZERO cb) | (ONE ca, MONE cb) | (MONE ca, ONE cb) -> ZERO (crazy2add (ca, cb))
	| (ZERO ca, ONE cb) | (ONE ca, ZERO cb) -> ONE (crazy2add (ca, cb))
	| (ZERO ca, MONE cb) | (MONE ca, ZERO cb) -> MONE (crazy2add (ca, cb))
	| (ONE ca, ONE cb) -> ZERO (crazy2add ((pc ca), cb))
	| (MONE ca, MONE cb) -> ZERO (crazy2add (ca, (mc cb)))