(*2016-11690*)
type crazy2 = NIL 
	| ZERO of crazy2
	| ONE of crazy2
	| MONE of crazy2

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (cra1,cra2) ->
	match cra1 with
	| NIL -> cra2
	| ZERO num1 -> (
		 match cra2 with
		| NIL -> cra1
		| ZERO num2 -> ZERO (crazy2add (num1,num2))
		| ONE num2 -> ONE (crazy2add (num1,num2))
		| MONE num2 -> MONE (crazy2add (num1,num2))
	)
	| ONE num1 -> (
		match cra2 with
		| NIL -> cra1
		| ZERO num2 -> ONE (crazy2add (num1,num2))
		| ONE num2 -> ZERO (crazy2add (ONE (NIL),crazy2add(num1,num2)))
		| MONE num2 -> ZERO (crazy2add (num1,num2))
	)
	| MONE num1 -> (
		match cra2 with
		| NIL -> cra1
		| ZERO num2 -> MONE (crazy2add (num1,num2)) 
		| ONE num2 -> ZERO (crazy2add (num1,num2))
		| MONE num2 -> ZERO (crazy2add (MONE (NIL),crazy2add(num1,num2)))
	)


(* test code 




let rec crazy2val : crazy2 -> int = fun num ->
	match num with
	| NIL -> 0
	| ZERO um -> 2 * (crazy2val um)
	| ONE um -> 2 * (crazy2val um) + 1
	| MONE um -> 2 * (crazy2val um) - 1

let _= 
let print_bool x = print_endline (string_of_bool x) in 

print_bool (0 = (crazy2val (crazy2add (ZERO NIL, ZERO NIL)))); 
print_bool (0 = (crazy2val (crazy2add (MONE NIL, ONE NIL)))); 
print_bool (1 = (crazy2val (crazy2add (ZERO NIL, ONE NIL)))); 
print_bool (4 = (crazy2val (crazy2add (ONE (ONE NIL), ONE NIL)))); 
print_bool (-683 = (crazy2val (crazy2add (MONE (ZERO (ZERO (ZERO NIL))), (ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL))))))))))))))) 
;; 
let rec crazy2val : crazy2 -> int = fun num ->
	match num with
	| NIL -> 0
	| ZERO um -> 2 * (crazy2val um)
	| ONE um -> 2 * (crazy2val um) + 1
	| MONE um -> 2 * (crazy2val um) - 1

*)