
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2


type carry = Z | P | M

let rec crazy2addwithcarry t = match t with
	| (NIL, x, Z) | (x, NIL, Z) -> x
	| (NIL, x, P) | (x, NIL, P) -> crazy2addwithcarry (x, ONE NIL, Z)
	| (NIL, x, M) | (x, NIL, M) -> crazy2addwithcarry (x, MONE NIL, Z)
	
(* 0 /1 *)
	| (ZERO x, ZERO y, Z)
(* 0 -1 1 /6 *)
	| (ONE x, MONE y, Z) | (ONE x, ZERO y, M)
	| (ZERO x, ONE y, M) | (ZERO x, MONE y, P)
	| (MONE x, ONE y, Z) | (MONE x, ZERO y, P) -> ZERO( crazy2addwithcarry(x, y, Z))

(*0 0 1 /3*)
	| (ZERO x, ZERO y, P) | (ZERO x, ONE y, Z) | (ONE x, ZERO y, Z)
(*1 1 -1 /3*)
	| (ONE x, ONE y, M) | (ONE x, MONE y, P) | (MONE x, ONE y, P) -> ONE( crazy2addwithcarry(x, y, Z))

(*0 1 1 /3*)
	| (ZERO x, ONE y, P) | (ONE x, ZERO y, P) | (ONE x, ONE y, Z) -> ZERO( crazy2addwithcarry(x, y, P))
(*1 1 1 /1*)
	| (ONE x, ONE y, P) -> ONE( crazy2addwithcarry(x, y, P));


(*0 0 -1 /3*)
	| (ZERO x, ZERO y, M) | (ZERO x, MONE y, Z) | (MONE x, ZERO y, Z)
(*-1 -1 1 /3*)
	| (MONE x, MONE y, P) | (ONE x, MONE y, M) | (MONE x, ONE y, M) -> MONE( crazy2addwithcarry(x, y, Z))

(*0 -1 -1 /3*)
	| (ZERO x, MONE y, M) | (MONE x, ZERO y, M) | (MONE x, MONE y, Z) -> ZERO( crazy2addwithcarry(x, y, M))

(*-1 -1 -1 /1*)
	| (MONE x, MONE y, M) -> MONE (crazy2addwithcarry(x, y, M))

let crazy2add (x, y) = crazy2addwithcarry (x, y, Z)
