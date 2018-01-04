
(* EXERCISE 2,3 *)
type crazy2 = NIL
	| ZERO of crazy2
	| ONE of crazy2
	| MONE of crazy2

let rec crazy2val : crazy2 -> int = fun input ->
	match input with
	| ZERO (subin) -> 2 * (crazy2val subin)
	| ONE (subin) -> 1 + 2 * (crazy2val subin)
	| MONE (subin) -> -1 + 2 * (crazy2val subin)
	| NIL -> 0

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (in1 , in2) ->
	match in1 with
	| NIL -> in2
	| ZERO (subin1) -> 
		(match in2 with
		| NIL -> ZERO (subin1)
		| ZERO (subin2) -> ZERO (crazy2add (subin1, subin2))
		| ONE (subin2) -> ONE (crazy2add (subin1, subin2))
		| MONE (subin2) -> MONE (crazy2add (subin1, subin2)))
	| ONE (subin1) ->
		(match in2 with
		| NIL -> ONE (subin1)
		| ZERO (subin2) -> ONE (crazy2add (subin1, subin2))
		| ONE (subin2) -> ZERO (crazy2add(ONE(NIL), crazy2add (subin1, subin2)))
		| MONE (subin2) -> ZERO (crazy2add (subin1, subin2)))
	| MONE (subin1) ->			
		(match in2 with
		| NIL -> MONE (subin1)
		| ZERO (subin2) -> MONE (crazy2add (subin1, subin2))
		| ONE (subin2) -> ZERO (crazy2add (subin1, subin2))
		| MONE (subin2) -> ZERO (crazy2add(MONE(NIL), crazy2add (subin1, subin2))))

(*
let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool(0 == crazy2val (ZERO(NIL)));
print_bool(7 == crazy2val (ONE(ONE(ONE(NIL)))));
print_bool(-7 == crazy2val (MONE(MONE(MONE(NIL)))));
print_bool(10 == crazy2val (ZERO(ONE(ZERO(ONE(NIL))))));
print_bool(-10 == crazy2val (ZERO(MONE(ZERO(MONE(NIL))))));
print_bool(0 == crazy2val (ZERO(ZERO(ZERO(ZERO(NIL))))));
print_bool(-5 == crazy2val (ONE(ONE(ZERO(MONE(NIL))))));
print_bool(-6 == crazy2val(crazy2add (ZERO(NIL), ZERO(MONE(MONE(NIL))))));
print_bool(22 == crazy2val(crazy2add (ONE(ONE(ONE(NIL))),ONE(ONE(ONE(ONE(NIL)))))));
print_bool(-22 == crazy2val(crazy2add (MONE(MONE(MONE(NIL))),MONE(MONE(MONE(MONE(NIL)))))));
print_bool(4 == crazy2val(crazy2add (ZERO(ONE(ZERO(ONE(MONE(NIL))))), ZERO(ONE(ZERO(ONE(NIL)))))));
print_bool(-4 == crazy2val(crazy2add (ZERO(MONE(ZERO(MONE(ONE(NIL))))), ZERO(MONE(ZERO(MONE(NIL)))))));
print_bool(0 == crazy2val(crazy2add (ZERO(ZERO(ZERO(ZERO(ZERO(NIL))))), ZERO(ZERO(ZERO(ZERO(NIL)))))));
print_bool(-10 == crazy2val(crazy2add (ONE(ONE(ZERO(MONE(NIL)))), ONE(ONE(ZERO(MONE(NIL)))))));
*)
