type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add : (crazy2 * crazy2) -> crazy2 = fun (cry1, cry2) ->
	match (cry1, cry2) with
	| (NIL, _) -> cry2
	| (_, NIL) -> cry1
	| (ZERO cry1_, ZERO cry2_) -> ZERO(crazy2add(cry1_, cry2_))
	| (ZERO cry1_, ONE cry2_) -> ONE(crazy2add(cry1_, cry2_))
	| (ZERO cry1_, MONE cry2_) -> MONE(crazy2add(cry1_, cry2_))
	| (ONE cry1_, ZERO cry2_) -> ONE(crazy2add(cry1_, cry2_))
	| (ONE cry1_, ONE cry2_) -> ZERO(crazy2add(ONE NIL, crazy2add(cry1_, cry2_)))
	| (ONE cry1_, MONE cry2_) -> ZERO(crazy2add(cry1_, cry2_))
	| (MONE cry1_, ZERO cry2_) -> MONE(crazy2add(cry1_, cry2_))
	| (MONE cry1_, ONE cry2_) -> ZERO(crazy2add(cry1_, cry2_))
	| (MONE cry1_, MONE cry2_) -> ZERO(crazy2add(MONE NIL, crazy2add(cry1_, cry2_)))
