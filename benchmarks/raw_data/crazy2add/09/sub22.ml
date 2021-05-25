type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

exception Error of string

let crazy2add(a,b) =
	if (a = NIL) || (b = NIL) then raise(Error "input is NIL")
	else
  	let rec crazy2add_c(a,b,carry) =
		match a with
  			ZERO(c) -> (match b with
  					ZERO(d) -> (match carry with
							ONE(temp) -> ONE(crazy2add_c(c,d,NIL))
							| MONE(temp) -> MONE(crazy2add_c(c,d,NIL))
							| _ -> ZERO(crazy2add_c(c,d,NIL)))
  					| ONE(d) -> (match carry with
							ONE(temp) -> ZERO(crazy2add_c(c,d,ONE(NIL)))
							| MONE(temp) -> ZERO(crazy2add_c(c,d,NIL))
							| _ -> ONE(crazy2add_c(c,d,NIL)))
					| MONE(d) -> (match carry with
							ONE(temp) -> ZERO(crazy2add_c(c,d,NIL))
							| MONE(temp) -> ZERO(crazy2add_c(c,d,MONE(NIL)))
							| _ -> MONE(crazy2add_c(c,d,NIL)))
					| _ -> (match carry with
							ONE(temp) -> ONE(crazy2add_c(c,NIL,NIL))
							| MONE(temp) -> MONE(crazy2add_c(c,NIL,NIL))
							| _ -> ZERO(crazy2add_c(c,NIL,NIL))))
  			| ONE(c) -> (match b with
  					ZERO(d) -> (match carry with
							ONE(temp) -> ZERO(crazy2add_c(c,d,ONE(NIL)))
							| MONE(temp) -> ZERO(crazy2add_c(c,d,NIL))
							| _ -> ONE(crazy2add_c(c,d,NIL)))
  					| ONE(d) -> (match carry with
							ONE(temp) -> ONE(crazy2add_c(c,d,ONE(NIL)))
							| MONE(temp) -> ONE(crazy2add_c(c,d,NIL))
							| _ -> ZERO(crazy2add_c(c,d,ONE(NIL))))
					| MONE(d) -> (match carry with
							ONE(temp) -> ONE(crazy2add_c(c,d,NIL))
							| MONE(temp) -> MONE(crazy2add_c(c,d,NIL))
							| _ -> ZERO(crazy2add_c(c,d,NIL)))
					| _ -> (match carry with
							ONE(temp) -> ZERO(crazy2add_c(c,NIL,ONE(NIL)))
							| MONE(temp) -> ZERO(crazy2add_c(c,NIL,NIL))
							| _ -> ONE(crazy2add_c(c,NIL,NIL))))
  			| MONE(c) -> (match b with
  					ZERO(d) -> (match carry with
							ONE(temp) -> ZERO(crazy2add_c(c,d,NIL))
							| MONE(temp) -> ZERO(crazy2add_c(c,d,MONE(NIL)))
							| _ -> MONE(crazy2add_c(c,d,NIL)))
  					| ONE(d) -> (match carry with
							ONE(temp) -> ZERO(crazy2add_c(c,d,ONE(NIL)))
							| MONE(temp) -> MONE(crazy2add_c(c,d,NIL))
							| _ -> ZERO(crazy2add_c(c,d,ONE(NIL))))
					| MONE(d) -> (match carry with
							ONE(temp) -> MONE(crazy2add_c(c,d,NIL))
							| MONE(temp) -> MONE(crazy2add_c(c,d,MONE(NIL)))
							| _ -> ZERO(crazy2add_c(c,d,MONE(NIL))))
					| _ -> (match carry with
							ONE(temp) -> ZERO(crazy2add_c(c,NIL,NIL))
							| MONE(temp) -> ZERO(crazy2add_c(c,NIL,MONE(NIL)))
							| _ -> MONE(crazy2add_c(c,NIL,NIL))))	
			| _ -> (match b with
  					ZERO(d) -> (match carry with
							ONE(temp) -> ONE(crazy2add_c(NIL,d,NIL))
							| MONE(temp) -> MONE(crazy2add_c(NIL,d,NIL))
							| _ -> ZERO(crazy2add_c(NIL,d,NIL)))
  					| ONE(d) -> (match carry with
							ONE(temp) -> ZERO(crazy2add_c(NIL,d,ONE(NIL)))
							| MONE(temp) -> ZERO(crazy2add_c(NIL,d,NIL))
							| _ -> ONE(crazy2add_c(NIL,d,ONE(NIL))))
					| MONE(d) -> (match carry with
							ONE(temp) -> ZERO(crazy2add_c(NIL,d,NIL))
							| MONE(temp) -> ZERO(crazy2add_c(NIL,d,MONE(NIL)))
							| _ -> MONE(crazy2add_c(NIL,d,NIL)))	
					| _ -> (match carry with
							ONE(temp) -> ONE(NIL)
							| MONE(temp) -> MONE(NIL)
							| _ -> NIL )) 
	in
	let rec filter x = 
		match x with
			NIL -> x
			| ZERO(y) -> if (filter y) = ZERO(NIL) then ZERO(NIL) else x
			| ONE(y) -> ONE(filter y)
			| MONE(y) -> MONE(filter y)
	in
	filter (crazy2add_c(a,b,NIL));;