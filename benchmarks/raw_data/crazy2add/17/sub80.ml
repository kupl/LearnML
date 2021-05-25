type crazy2 =    NIL
                | ZERO of crazy2
                | ONE of crazy2
                | MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 =  fun(first_crazy, second_crazy) 
-> 
	(
	   match first_crazy with
		  NIL -> second_crazy
		| ZERO(crazy1_) -> 
			(
			  match second_crazy with
			 	 NIL -> first_crazy
				|ZERO(crazy2_) -> ZERO(crazy2add(crazy1_,crazy2_))
				|ONE(crazy2_) -> ONE(crazy2add(crazy1_,crazy2_))
				|MONE(crazy2_) -> MONE(crazy2add(crazy1_,crazy2_))
			)
		| ONE(crazy1_) ->
			(
			  match second_crazy with
				 NIL -> first_crazy
				|ZERO(crazy2_) -> ONE(crazy2add(crazy1_,crazy2_))
				|ONE(crazy2_) -> ZERO(crazy2add(ONE(NIL),crazy2add(crazy1_,crazy2_)))
				|MONE(crazy2_) -> ZERO(crazy2add(crazy1_,crazy2_)) 
			)
		| MONE(crazy1_) ->
			(
			  match second_crazy with
				 NIL -> first_crazy
				|ZERO(crazy2_) -> MONE(crazy2add(crazy1_,crazy2_))
				|ONE(crazy2_) -> ZERO(crazy2add(crazy1_,crazy2_))
				|MONE(crazy2_) -> ZERO(crazy2add(MONE(NIL),crazy2add(crazy1_,crazy2_)))
			)
	)
