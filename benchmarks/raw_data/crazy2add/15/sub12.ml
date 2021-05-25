type crazy2 = NIL
			| ZERO of crazy2
			| ONE of crazy2
			| MONE of crazy2


let rec crazy2add ((cv1: crazy2), (cv2: crazy2)): crazy2 =
	match (cv1, cv2) with

	| (NIL, _) -> cv2
	| (_, NIL) -> cv1
	| (ZERO cv1', ZERO cv2') -> ZERO (crazy2add (cv1', cv2'))
	| (ZERO cv1', ONE cv2') -> ONE (crazy2add (cv1', cv2'))
	| (ZERO cv1', MONE cv2') -> MONE (crazy2add (cv1', cv2'))
	| (ONE cv1', ZERO cv2') -> ONE (crazy2add (cv1', cv2'))
	| (ONE cv1', ONE cv2') -> ZERO (crazy2add ((ONE NIL), crazy2add (cv1', cv2')))
	| (ONE cv1', MONE cv2') -> ZERO (crazy2add (cv1', cv2'))
	| (MONE cv1', ZERO cv2') -> MONE (crazy2add (cv1', cv2'))
	| (MONE cv1', ONE cv2') -> ZERO (crazy2add (cv1', cv2'))
	| (MONE cv1', MONE cv2') -> ZERO (crazy2add ((MONE NIL), crazy2add (cv1', cv2')))
