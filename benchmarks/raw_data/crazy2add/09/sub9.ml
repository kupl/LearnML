(* 2006-11782 Song Young-chan, Hw1-7 crazy2add *)

exception Error of string

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add_in (number1, number2) =
	match (number1,number2) with
	 (NIL,NIL) -> NIL
	|(NIL,MONE(remain)) | (MONE(remain),NIL) -> MONE(crazy2add_in (NIL,remain))
	|(NIL,ONE(remain)) | (ONE(remain),NIL) -> ONE(crazy2add_in (NIL,remain))
	|(NIL,ZERO(remain)) | (ZERO(remain),NIL) -> ZERO(crazy2add_in (NIL,remain))
	|((MONE(remain1)), (ONE(remain2))) | ((ONE(remain1)), (MONE(remain2))) | ((ZERO(remain1)), (ZERO(remain2))) -> ZERO(crazy2add_in (remain1,remain2))
	|((MONE(remain1)), (ZERO(remain2))) | ((ZERO(remain1)), (MONE(remain2))) -> MONE(crazy2add_in (remain1,remain2))
	|((ONE(remain1)), (ZERO(remain2))) | ((ZERO(remain1)), (ONE(remain2))) -> ONE(crazy2add_in (remain1,remain2))
	|((MONE(remain1)), (MONE(remain2))) -> ZERO(crazy2add_in ((crazy2add_in ((MONE NIL),remain1)), remain2))
	|((ONE(remain1)), (ONE(remain2))) -> ZERO(crazy2add_in ((crazy2add_in ((ONE NIL),remain1)), remain2))
	 
let crazy2add ((number1:crazy2),(number2:crazy2)) = 
	match (number1,number2) with
	 (NIL, _) | (_, NIL) -> raise (Error "invalid arg")
	| _ -> crazy2add_in (number1, number2)

let a = ONE (ZERO (ONE NIL))
let b = MONE (ZERO (ZERO NIL))
