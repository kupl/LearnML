type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val = fun crazy2 ->
	match crazy2 with
	|NIL -> 0
	|ZERO crazy -> 2 * (crazy2val crazy)
	|ONE crazy -> 1 + 2 * (crazy2val crazy)
	|MONE crazy -> -1 + 2 * (crazy2val crazy)

let rec crazy2add = fun (lc, rc) -> 
	match (lc, rc) with
	|(NIL, _) -> rc
	|(_, NIL) -> lc
	|(ZERO lt, ZERO rt) -> ZERO (crazy2add(lt, rt))
	|(ZERO lt, ONE rt) -> ONE (crazy2add(lt, rt))
	|(ZERO lt, MONE rt) -> MONE (crazy2add(lt, rt))
	|(ONE lt, ZERO rt) -> ONE (crazy2add(lt, rt))
	|(ONE lt, ONE rt) -> ZERO (crazy2add((crazy2add(ONE NIL, lt)), rt))
	|(ONE lt, MONE rt) -> ZERO (crazy2add(lt, rt))
	|(MONE lt, ZERO rt) -> MONE (crazy2add(lt, rt))
	|(MONE lt, ONE rt) -> ZERO (crazy2add(lt, rt))
	|(MONE lt, MONE rt) -> ZERO (crazy2add((crazy2add(MONE NIL, lt)), rt))
