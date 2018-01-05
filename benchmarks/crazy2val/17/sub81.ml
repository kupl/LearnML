type crazy2 = 	 NIL
	    	| ZERO of crazy2
		| ONE of crazy2
		| MONE of crazy2

let rec crazy2val: crazy2 -> int = fun(crazy2_input) ->
		 ( 
		   match crazy2_input with
			 NIL -> 0
			|ZERO(crazy2_) -> 2 * crazy2val(crazy2_)
			|ONE(crazy2_) -> 1 + 2 * crazy2val(crazy2_)
			|MONE(crazy2_) -> -1 + 2 * crazy2val(crazy2_)	
		 )
