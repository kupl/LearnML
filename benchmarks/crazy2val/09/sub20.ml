exception Error of string
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2val c=
	let rec crazy2val_sub c3=
		match c3 with
					NIL->0
					|ZERO(c1)->crazy2val_sub c1*2
					| ONE(c1)->(crazy2val_sub c1)*2+1
					| MONE(c1)->(crazy2val_sub c1)*2-1
	in		
	if c=NIL then raise (Error("invalid arg"))
	else crazy2val_sub c
			