type crazy2 = NIL
			| ZERO of crazy2
			| ONE of crazy2
			| MONE of crazy2

let rec crazy2val (cv: crazy2): int =
	match cv with
	| NIL -> 0
	| ZERO cv' -> 0 + (2 * crazy2val cv')
	| ONE cv' -> 1 + (2 * crazy2val cv')
	| MONE cv' -> -1 + (2 * crazy2val cv')

(* using test *)
(*
let _ =
	let msg = string_of_int (crazy2val (ZERO(ONE(MONE NIL)))) in
	print_endline msg

let _ =
	let msg = string_of_int (crazy2val (ONE NIL)) in
	print_endline msg

let _ =
	let msg = string_of_int (crazy2val (ONE(ZERO(ONE NIL)))) in
	print_endline msg

let _ =
	let msg = string_of_int (crazy2val (ONE(MONE NIL))) in
	print_endline msg

let _ =
	let msg = string_of_int (crazy2val (ONE(MONE(ZERO(MONE NIL))))) in
	print_endline msg
*)
(*
let _= 
let print_bool x = print_endline (string_of_bool x) in 
print_bool (-1  = (crazy2val (MONE NIL))); 
print_bool (1   = (crazy2val (ONE (ZERO (ZERO (ZERO NIL)))))); 
print_bool (1   = (crazy2val (ONE NIL))); 
print_bool (9   = (crazy2val (MONE (MONE (ONE (ONE (ZERO NIL))))))); 
print_bool (-13 = (crazy2val (MONE (ZERO (ONE (ZERO (ONE (MONE NIL)))))))) 
;; 
*)