type crazy2 = NIL
						| ZERO of crazy2
						| ONE of crazy2
						| MONE of crazy2
let count = 0

let rec crazy2val (arg: crazy2) = 
	match arg with 
	| NIL -> count
	| ZERO(arg') -> count + (2 * crazy2val(arg')) 
	| ONE(arg') -> count + (2 * crazy2val(arg')) + 1
	| MONE(arg') -> count + (2 * crazy2val(arg')) - 1

(*let x = ONE(ZERO(ONE NIL))*)

(*let _ = print_int (crazy2val x)*)


