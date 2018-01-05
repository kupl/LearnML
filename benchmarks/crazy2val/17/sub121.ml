(*2016-11690*)
type crazy2 = NIL 
	| ZERO of crazy2
	| ONE of crazy2
	| MONE of crazy2

let rec crazy2val : crazy2 -> int = fun num ->
	match num with
	| NIL -> 0
	| ZERO um -> 2 * (crazy2val um)
	| ONE um -> 2 * (crazy2val um) + 1
	| MONE um -> 2 * (crazy2val um) - 1

(*testcode







let _= 
let print_bool x = print_endline (string_of_bool x) in 
print_bool (-1  = (crazy2val (MONE NIL))); 
print_bool (1   = (crazy2val (ONE (ZERO (ZERO (ZERO NIL)))))); 
print_bool (1   = (crazy2val (ONE NIL))); 
print_bool (9   = (crazy2val (MONE (MONE (ONE (ONE (ZERO NIL))))))); 
print_bool (-13 = (crazy2val (MONE (ZERO (ONE (ZERO (ONE (MONE NIL)))))))) 
;;
 
*)