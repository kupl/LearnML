(*2006-11720 Kim Eunsol HW1 #5*)

type nat = ZERO | SUCC of nat

let rec natadd(a,b) = 
	match a with SUCC c ->(
			 match b with ZERO -> SUCC(natadd(c, ZERO))
	 		 |SUCC d -> SUCC(SUCC(natadd(c, d)))
		   )
	|ZERO->(
			match b with ZERO -> ZERO
			|SUCC d -> SUCC(natadd(ZERO,d))
		   )

let rec natmul(a, b) = 
	match a with SUCC c -> natadd(b, natmul(c, b))
	|ZERO -> ZERO
