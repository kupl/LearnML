type crazy2 = NIL
			| ZERO of crazy2
			| ONE of crazy2
			| MONE of crazy2

(* using test *)
(*
let rec crazy2val (cv: crazy2): int =
	match cv with
	| NIL -> 0
	| ZERO cv' -> 0 + (2 * crazy2val cv')
	| ONE cv' -> 1 + (2 * crazy2val cv')
	| MONE cv' -> -1 + (2 * crazy2val cv')
*)

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

(*
let _ = print_endline (string_of_int (crazy2val (crazy2add (MONE NIL, ONE NIL))))

let _= 
let print_bool x = print_endline (string_of_bool x) in 

print_bool (0 = (crazy2val (crazy2add (ZERO NIL, ZERO NIL)))); 
print_bool (0 = (crazy2val (crazy2add (MONE NIL, ONE NIL)))); 
print_bool (1 = (crazy2val (crazy2add (ZERO NIL, ONE NIL)))); 
print_bool (4 = (crazy2val (crazy2add (ONE (ONE NIL), ONE NIL)))); 
print_bool (-683 = (crazy2val (crazy2add (MONE (ZERO (ZERO (ZERO NIL))), (ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL))))))))))))))) 
;; 
*)