exception Error of string
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2
let crazy2add (a, b)=
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
	in
	let rec val2crazy c=
		if c=0 then NIL
		else if c>0 then 
			match c mod 2 with
				| 0 ->ZERO(val2crazy (c/2))
				| _->ONE(val2crazy ((c-1)/2))
		else 
			match c mod 2 with 
				| 0 ->ZERO(val2crazy (c/2))
				| _ ->MONE(val2crazy ((c+1)/2))
	in
	if (crazy2val a)+(crazy2val b)=0 then ZERO(NIL)
	else 	val2crazy ((crazy2val a)+(crazy2val b))
		
			
			
		