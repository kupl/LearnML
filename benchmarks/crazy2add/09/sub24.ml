type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

exception Error of string

let rec crazy2add(ain,bin) =
	let rec filter x = 
		match x with
			NIL -> x
			| ZERO(y) -> if (filter y) = ZERO(NIL) then ZERO(NIL) else x
			| ONE(y) -> ONE(filter y)
			| MONE(y) -> MONE(filter y)
	in
	let rec adder(a,b) =
		match a with
			ZERO(x) -> (match b with ONE(y) -> ONE(adder(x,y)) | MONE(y) -> MONE(adder(x,y)) | ZERO(y) -> ZERO(adder(x,y)) | _ -> a)
			| ONE(x) -> (match b with ONE(y) -> ZERO(adder(ONE(NIL),(adder(x,y)))) | MONE(y) -> ZERO(adder(x,y)) | ZERO(y) -> ONE(adder(x,y)) | _ -> a)
			| MONE(x) -> (match b with ONE(y) -> ZERO(adder(x,y)) | MONE(y) -> ZERO(adder(MONE(NIL),(adder(x,y)))) | ZERO(y) -> MONE(adder(x,y)) | _ -> a)
			| _ -> b
	in
	filter(adder(ain,bin));;
