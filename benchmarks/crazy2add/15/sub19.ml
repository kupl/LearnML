type crazy2 = NIL
			| ZERO of crazy2
			| ONE of crazy2
			| MONE of crazy2

let rec crazy2add (ca, cb) = crazy2add_in ca cb (ZERO(NIL))
	and crazy2add_in a b carry = 
		match a with
		NIL	-> (
			match b with
			NIL	-> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> NIL
				|ONE(f)     -> ONE(NIL)
				|MONE(f)    -> MONE(NIL)
				)
			|ZERO(e)	-> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> ZERO(crazy2add_in a e (ZERO(NIL)))
				|ONE(f)     -> ONE(crazy2add_in a e (ZERO(NIL)))
				|MONE(f)    -> MONE(crazy2add_in a e (ZERO(NIL)))
				)
			|ONE(e)	-> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> ONE(crazy2add_in a e (ZERO(NIL)))
				|ONE(f)     -> ZERO(crazy2add_in a e (ONE(NIL)))
				|MONE(f)    -> ZERO(crazy2add_in a e (ZERO(NIL)))
				)
			|MONE(e)	->(
				match carry with
				NIL         -> NIL  
				|ZERO(f)    -> MONE(crazy2add_in a e (ZERO(NIL)))
				|ONE(f)     -> ZERO(crazy2add_in a e (ZERO(NIL)))
				|MONE(f)    -> ZERO(crazy2add_in a e (MONE(NIL)))
				)
		)
		|ZERO(d)	-> (
			match b with
			NIL         -> (
				match carry with
				NIL         -> NIL  
				|ZERO(f)    -> ZERO(crazy2add_in d b (ZERO(NIL)))
				|ONE(f)     -> ONE(crazy2add_in d b (ZERO(NIL)))
				|MONE(f)    -> MONE(crazy2add_in d b (ZERO(NIL)))
				)
			|ZERO(e)    -> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> ZERO(crazy2add_in d e (ZERO(NIL)))
				|ONE(f)     -> ONE(crazy2add_in d e (ZERO(NIL)))
				|MONE(f)    -> MONE(crazy2add_in d e (ZERO(NIL)))
				)
			|ONE(e)     -> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> ONE(crazy2add_in d e (ZERO(NIL)))
				|ONE(f)     -> ZERO(crazy2add_in d e (ONE(NIL)))
				|MONE(f)    -> ZERO(crazy2add_in d e (ZERO(NIL)))
				)
			|MONE(e)    -> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> MONE(crazy2add_in d e (ZERO(NIL)))
				|ONE(f)     -> ZERO(crazy2add_in d e (ZERO(NIL)))
				|MONE(f)    -> ZERO(crazy2add_in d e (MONE(NIL)))
				)
			)
		|ONE(d)		-> (
			match b with
			NIL         -> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> ONE(crazy2add_in d b (ZERO(NIL)))
				|ONE(f)     -> ZERO(crazy2add_in d b (ONE(NIL)))
				|MONE(f)    -> ZERO(crazy2add_in d b (ZERO(NIL)))
				)
			|ZERO(e)    -> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> ONE(crazy2add_in d e (ZERO(NIL)))
				|ONE(f)     -> ZERO(crazy2add_in d e (ONE(NIL)))
			   	|MONE(f)    -> ZERO(crazy2add_in d e (ZERO(NIL)))
				)
			|ONE(e)     -> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> ZERO(crazy2add_in d e (ONE(NIL)))
				|ONE(f)     -> ONE(crazy2add_in d e (ONE(NIL)))
				|MONE(f)    -> ONE(crazy2add_in d e (ZERO(NIL)))
				)
			|MONE(e)    -> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> ZERO(crazy2add_in d e (ZERO(NIL)))
				|ONE(f)     -> ONE(crazy2add_in d e (ZERO(NIL)))
				|MONE(f)    -> MONE(crazy2add_in d e (ZERO(NIL)))
				)
			)
		|MONE(d)	-> (
			match b with
			NIL			-> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> MONE(crazy2add_in d b (ZERO(NIL)))
				|ONE(f)     -> ZERO(crazy2add_in d b (ZERO(NIL)))
				|MONE(f)    -> ZERO(crazy2add_in d b (MONE(NIL)))
				)
			|ZERO(e)	-> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> ZERO(crazy2add_in d e (ZERO(NIL)))
				|ONE(f)     -> ZERO(crazy2add_in d e (ZERO(NIL)))
				|MONE(f)    -> ZERO(crazy2add_in d e (MONE(NIL)))
				)
			|ONE(e)		-> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> ZERO(crazy2add_in d e (ZERO(NIL)))
				|ONE(f)     -> ONE(crazy2add_in d e (ZERO(NIL)))
				|MONE(f)    -> MONE(crazy2add_in d e (ZERO(NIL)))
				)
			|MONE(e)	-> (
				match carry with
				NIL         -> NIL
				|ZERO(f)    -> ZERO(crazy2add_in d e (MONE(NIL)))
				|ONE(f)     -> MONE(crazy2add_in d e (ZERO(NIL)))
				|MONE(f)    -> MONE(crazy2add_in d e (MONE(NIL)))
				)
			)
