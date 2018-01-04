exception Error of string

(* EX7 : crazy2add *)
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2
(* definition of crazy2 *)

let rec crazy2add a b =
	(* function crazy2inc : increase the crazy2 *)
	let rec crazy2inc a =
		match a with
			NIL -> ONE NIL
			| ZERO x -> ONE x
			| ONE x -> ZERO ( crazy2inc x )
			| MONE x -> ZERO x in
	(* function crazy2dec : decrease the crazy2 *)
	let rec crazy2dec a =
		match a with
			NIL -> MONE NIL
			| ZERO x -> MONE x
			| ONE x -> ZERO x
			| MONE x -> ZERO ( crazy2dec x ) in
	(* function simpler : remove duplicate "ZERO" *)
	let rec simpler a =
		match a with
			NIL -> NIL
			| ZERO x -> if x = NIL then NIL
						else simpler ( ZERO ( simpler x  )) 
			| ONE x -> ONE ( simpler x )
			| MONE x -> MONE ( simpler x ) in
	(* function adder : add two crazy2s by case *)
	let rec adder a b =
		match a with
			NIL -> b 
			| ZERO x -> 
				( match b with
					NIL -> ZERO x
					| ZERO y -> ZERO ( adder x y )
					| ONE y -> ONE ( adder x y ) 
					| MONE y -> MONE ( adder x y ) ) 
			| ONE x ->
				( match b with
				  	NIL -> ONE x
					| ZERO y -> ONE ( adder x y ) 
					| ONE y -> ZERO ( crazy2inc ( adder x y ) )  
					| MONE y -> ZERO ( adder x y ) ) 
			| MONE x -> 
				( match b with
				  	NIL -> MONE x
					| ZERO y ->  MONE ( adder x y  ) 
					| ONE y ->  ZERO ( adder x y ) 
					| MONE y -> ZERO ( crazy2dec( adder x y ) ) )  in
	simpler( adder a b )
